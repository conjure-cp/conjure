{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Conjure.Language.Instantiate
    ( instantiateExpression
    , instantiateDomain
    , trySimplify
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.UserError
import Conjure.Language.Definition
import Conjure.Language.Expression.Op
import Conjure.Language.Domain
import Conjure.Language.Constant
import Conjure.Language.TypeOf
import Conjure.Language.Pretty
import Conjure.Process.Enumerate ( EnumerateDomain, enumerateDomain, enumerateInConstant )


-- | Try to simplify an expression recursively.
trySimplify :: (MonadUserError m, EnumerateDomain m) => Expression -> m Expression
trySimplify x = do
    res <- runMaybeT $ instantiateExpression [] x
    case res of
        Just c                                                  -- if the expression can be evaluated into a Constant
            | null [() | ConstantUndefined{} <- universe c]     -- and if it doesn't contain undefined's in it
            -> return (Constant c)                              -- evaluate to the constant
        _   -> descendM trySimplify x                           -- otherwise, try the same on its children


instantiateExpression
    :: (MonadFail m, EnumerateDomain m)
    => [(Name, Expression)]
    -> Expression
    -> m Constant
instantiateExpression ctxt x = do
    constant <- normaliseConstant <$> evalStateT (instantiateE x) ctxt
    case (emptyCollection constant, constant) of
        (_, TypedConstant{}) -> return constant
        (True, _) -> do
            ty <- typeOf x
            return (TypedConstant constant ty)
        (False, _) -> return constant


instantiateDomain
    :: ( MonadFail m
       , EnumerateDomain m
       , Pretty r
       , Default r
       )
    => [(Name, Expression)]
    -> Domain r Expression
    -> m (Domain r Constant)
instantiateDomain ctxt x = normaliseDomain normaliseConstant <$> evalStateT (instantiateD x) ctxt


newtype HasUndef = HasUndef Any
    deriving (Monoid)

instantiateE
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       , EnumerateDomain m
       )
    => Expression
    -> m Constant

instantiateE (Comprehension body gensOrConds) = do
    let
        loop :: ( MonadFail m
                , MonadState [(Name, Expression)] m
                , EnumerateDomain m
                ) => [GeneratorOrCondition] -> WriterT HasUndef m [Constant]
        loop [] = return <$> instantiateE body
        loop (Generator (GenDomainNoRepr pat domain) : rest) = do
            DomainInConstant domainConstant <- instantiateE (Domain domain)
            let undefinedsInsideTheDomain =
                    [ und
                    | und@ConstantUndefined{} <- universeBi domainConstant
                    ]
            if null undefinedsInsideTheDomain
                then do
                    enumeration <- enumerateDomain domainConstant
                    concatMapM
                        (\ val -> scope $ do
                            valid <- bind pat val
                            if valid
                                then loop rest
                                else return [] )
                        enumeration
                else do
                    tell (HasUndef (Any True))
                    return []
        loop (Generator (GenDomainHasRepr pat domain) : rest) =
            loop (Generator (GenDomainNoRepr (Single pat) (forgetRepr domain)) : rest)
        loop (Generator (GenInExpr pat expr) : rest) = do
            exprConstant <- instantiateE expr
            enumeration <- enumerateInConstant exprConstant
            concatMapM
                (\ val -> scope $ do
                    valid <- bind pat val
                    if valid
                        then loop rest
                        else return [] )
                enumeration
        loop (Condition expr : rest) = do
            constant <- instantiateE expr
            if constant == ConstantBool True
                then loop rest
                else return []
        loop (ComprehensionLetting n expr : rest) = do
            constant <- instantiateE expr
            valid <- bind (Single n) constant
            unless valid (bug "ComprehensionLetting.bind expected to be valid")
            loop rest


    (constants, HasUndef (Any undefinedsInsideGeneratorDomains)) <- runWriterT (loop gensOrConds)
    if undefinedsInsideGeneratorDomains
        then do
            ty <- typeOf (Comprehension body gensOrConds)
            return $ ConstantUndefined
                "Comprehension contains undefined values inside generator domains."
                ty
        else
            return $ ConstantAbstract $ AbsLitMatrix
                (DomainInt [RangeBounded 1 (fromInt (genericLength constants))])
                constants

instantiateE (Reference name (Just (RecordField _ ty))) = return $ ConstantField name ty
instantiateE (Reference name (Just (VariantField _ ty))) = return $ ConstantField name ty
instantiateE (Reference _    (Just (Alias x))) = instantiateE x
instantiateE (Reference name _) = do
    ctxt <- gets id
    case name `lookup` ctxt of
        Nothing -> fail $ vcat
            $ ("No value for:" <+> pretty name)
            : "Bindings in context:"
            : prettyContext ctxt
        Just x -> instantiateE x

instantiateE (Constant c) = return c
instantiateE (AbstractLiteral lit) = ConstantAbstract <$> instantiateAbsLit lit
instantiateE (Typed x ty) = TypedConstant <$> instantiateE x <*> pure ty
instantiateE (Op op) = instantiateOp op

-- "Domain () Expression"s inside expressions are handled specially
instantiateE (Domain (DomainReference _ (Just d))) = instantiateE (Domain d)
instantiateE (Domain (DomainReference name Nothing)) = do
    ctxt <- gets id
    case name `lookup` ctxt of
        Just (Domain d) -> instantiateE (Domain d)
        _ -> fail $ vcat
            $ ("No value for:" <+> pretty name)
            : "Bindings in context:"
            : prettyContext ctxt
instantiateE (Domain domain) = DomainInConstant <$> instantiateD domain

instantiateE (WithLocals b (AuxiliaryVars locals)) = do
    forM_ locals $ \ local -> case local of
        SuchThat xs -> forM_ xs $ \ x -> do
            constant <- instantiateE x
            case constant of
                ConstantBool True -> return ()
                _                 -> fail $ "local:" <+> pretty constant
        _ -> fail $ "local:" <+> pretty local
    instantiateE b

instantiateE (WithLocals b (DefinednessConstraints locals)) = do
    forM_ locals $ \ x -> do
            constant <- instantiateE x
            case constant of
                ConstantBool True -> return ()
                _                 -> fail $ "local:" <+> pretty constant
    instantiateE b

instantiateE x = fail $ "instantiateE:" <+> pretty (show x)


instantiateOp
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       , EnumerateDomain m
       )
    => Op Expression
    -> m Constant
instantiateOp opx = mapM instantiateE opx >>= evaluateOp . fmap normaliseConstant


instantiateAbsLit
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       , EnumerateDomain m
       )
    => AbstractLiteral Expression
    -> m (AbstractLiteral Constant)
instantiateAbsLit = mapM instantiateE


instantiateD
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       , EnumerateDomain m
       , Pretty r
       , Default r
       )
    => Domain r Expression
    -> m (Domain r Constant)
instantiateD (DomainAny t ty) = return (DomainAny t ty)
instantiateD DomainBool = return DomainBool
instantiateD (DomainIntE x) = do
    x' <- instantiateE x
    let vals = case (x', viewConstantMatrix x', viewConstantSet x') of
                (ConstantInt{}, _, _) -> [x']
                (_, Just (_, xs), _) -> xs
                (_, _, Just xs) -> xs
                _ -> []
    return (DomainInt (map RangeSingle vals))
instantiateD (DomainInt ranges) = DomainInt <$> mapM instantiateR ranges
instantiateD (DomainEnum nm Nothing _) = do
    st <- gets id
    case lookup nm st of
        Just (Domain dom) -> instantiateD (defRepr dom)
        Just _  -> fail $ ("DomainEnum not found in state, Just:" <+> pretty nm) <++> vcat (map pretty st)
        Nothing -> fail $ ("DomainEnum not found in state, Nothing:" <+> pretty nm) <++> vcat (map pretty st)
instantiateD (DomainEnum nm rs0 _) = do
    let fmap4 = fmap . fmap . fmap . fmap
    let e2c' x = either bug id (e2c x)
    rs <- transformBiM (\ x -> Constant <$> instantiateE x ) (rs0 :: Maybe [Range Expression])
                |> fmap4 e2c'
    st <- gets id
    mp <- forM (universeBi rs :: [Name]) $ \ n -> case lookup n st of
            Just (Constant (ConstantInt i)) -> return (n, i)
            Nothing -> fail $ "No value for member of enum domain:" <+> pretty n
            Just _  -> fail $ "Incompatible value for member of enum domain:" <+> pretty n
    return (DomainEnum nm (rs :: Maybe [Range Constant]) (Just mp))
instantiateD (DomainUnnamed nm s) = DomainUnnamed nm <$> instantiateE s
instantiateD (DomainTuple inners) = DomainTuple <$> mapM instantiateD inners
instantiateD (DomainRecord  inners) = DomainRecord  <$> sequence [ do d' <- instantiateD d ; return (n,d')
                                                                 | (n,d) <- inners ]
instantiateD (DomainVariant inners) = DomainVariant <$> sequence [ do d' <- instantiateD d ; return (n,d')
                                                                 | (n,d) <- inners ]
instantiateD (DomainMatrix index inner) = DomainMatrix <$> instantiateD index <*> instantiateD inner
instantiateD (DomainSet       r attrs inner) = DomainSet r <$> instantiateSetAttr attrs <*> instantiateD inner
instantiateD (DomainMSet      r attrs inner) = DomainMSet r <$> instantiateMSetAttr attrs <*> instantiateD inner
instantiateD (DomainFunction  r attrs innerFr innerTo) = DomainFunction r <$> instantiateFunctionAttr attrs <*> instantiateD innerFr <*> instantiateD innerTo
instantiateD (DomainSequence  r attrs inner) = DomainSequence r <$> instantiateSequenceAttr attrs <*> instantiateD inner
instantiateD (DomainRelation  r attrs inners) = DomainRelation r <$> instantiateRelationAttr attrs <*> mapM instantiateD inners
instantiateD (DomainPartition r attrs inner) = DomainPartition r <$> instantiatePartitionAttr attrs <*> instantiateD inner
instantiateD DomainOp{} = bug "instantiateD DomainOp"
instantiateD (DomainReference _ (Just d)) = instantiateD d
instantiateD (DomainReference name Nothing) = do
    ctxt <- gets id
    case name `lookup` ctxt of
        Just (Domain d) -> instantiateD (defRepr d)
        _ -> fail $ vcat
            $ ("No value for:" <+> pretty name)
            : "Bindings in context:"
            : prettyContext ctxt
instantiateD DomainMetaVar{} = bug "instantiateD DomainMetaVar"


instantiateSetAttr
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       , EnumerateDomain m
       )
    => SetAttr Expression
    -> m (SetAttr Constant)
instantiateSetAttr (SetAttr s) = SetAttr <$> instantiateSizeAttr s


instantiateSizeAttr
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       , EnumerateDomain m
       )
    => SizeAttr Expression
    -> m (SizeAttr Constant)
instantiateSizeAttr SizeAttr_None = return SizeAttr_None
instantiateSizeAttr (SizeAttr_Size x) = SizeAttr_Size <$> instantiateE x
instantiateSizeAttr (SizeAttr_MinSize x) = SizeAttr_MinSize <$> instantiateE x
instantiateSizeAttr (SizeAttr_MaxSize x) = SizeAttr_MaxSize <$> instantiateE x
instantiateSizeAttr (SizeAttr_MinMaxSize x y) = SizeAttr_MinMaxSize <$> instantiateE x <*> instantiateE y


instantiateMSetAttr
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       , EnumerateDomain m
       )
    => MSetAttr Expression
    -> m (MSetAttr Constant)
instantiateMSetAttr (MSetAttr s o) = MSetAttr <$> instantiateSizeAttr s <*> instantiateOccurAttr o


instantiateOccurAttr
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       , EnumerateDomain m
       )
    => OccurAttr Expression
    -> m (OccurAttr Constant)
instantiateOccurAttr OccurAttr_None = return OccurAttr_None
instantiateOccurAttr (OccurAttr_MinOccur x) = OccurAttr_MinOccur <$> instantiateE x
instantiateOccurAttr (OccurAttr_MaxOccur x) = OccurAttr_MaxOccur <$> instantiateE x
instantiateOccurAttr (OccurAttr_MinMaxOccur x y) = OccurAttr_MinMaxOccur <$> instantiateE x <*> instantiateE y


instantiateFunctionAttr
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       , EnumerateDomain m
       )
    => FunctionAttr Expression
    -> m (FunctionAttr Constant)
instantiateFunctionAttr (FunctionAttr s p j) =
    FunctionAttr <$> instantiateSizeAttr s
                 <*> pure p
                 <*> pure j


instantiateSequenceAttr
    :: ( MonadFail m
       , MonadUserError m
       , MonadState [(Name, Expression)] m
       , EnumerateDomain m
       )
    => SequenceAttr Expression
    -> m (SequenceAttr Constant)
instantiateSequenceAttr (SequenceAttr s j) =
    SequenceAttr <$> instantiateSizeAttr s
                 <*> pure j


instantiateRelationAttr
    :: ( MonadFail m
       , MonadUserError m
       , MonadState [(Name, Expression)] m
       , EnumerateDomain m
       )
    => RelationAttr Expression
    -> m (RelationAttr Constant)
instantiateRelationAttr (RelationAttr s b) = RelationAttr <$> instantiateSizeAttr s <*> pure b


instantiatePartitionAttr
    :: ( MonadFail m
       , MonadUserError m
       , MonadState [(Name, Expression)] m
       , EnumerateDomain m
       )
    => PartitionAttr Expression
    -> m (PartitionAttr Constant)
instantiatePartitionAttr (PartitionAttr a b c) =
    PartitionAttr <$> instantiateSizeAttr a
                  <*> instantiateSizeAttr b
                  <*> pure c


instantiateR
    :: ( MonadFail m
       , MonadUserError m
       , MonadState [(Name, Expression)] m
       , EnumerateDomain m
       )
    => Range Expression
    -> m (Range Constant)
instantiateR RangeOpen = return RangeOpen
instantiateR (RangeSingle x) = RangeSingle <$> instantiateE x
instantiateR (RangeLowerBounded x) = RangeLowerBounded <$> instantiateE x
instantiateR (RangeUpperBounded x) = RangeUpperBounded <$> instantiateE x
instantiateR (RangeBounded x y) = RangeBounded <$> instantiateE x <*> instantiateE y


bind :: (Functor m, MonadState [(Name, Expression)] m)
    => AbstractPattern
    -> Constant
    -> m Bool -- False means skip
bind (Single nm) val = modify ((nm, Constant val) :) >> return True
bind (AbsPatTuple  pats) (ConstantAbstract (AbsLitTuple    vals))
    | length pats == length vals = and <$> zipWithM bind pats vals
bind (AbsPatMatrix pats) (ConstantAbstract (AbsLitMatrix _ vals))
    | length pats == length vals = and <$> zipWithM bind pats vals
bind (AbsPatSet    pats) (ConstantAbstract (AbsLitSet      vals))
    | length pats == length vals = and <$> zipWithM bind pats vals
    | otherwise                  = return False
bind pat val = bug $ "Instantiate.bind:" <++> vcat ["pat:" <+> pretty pat, "val:" <+> pretty val]
