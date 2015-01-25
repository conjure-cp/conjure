{-# LANGUAGE FlexibleContexts #-}

module Conjure.Language.Instantiate
    ( instantiateExpression
    , instantiateDomain
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Ops
import Conjure.Language.Domain
import Conjure.Language.Constant
import Conjure.Language.Pretty
import Conjure.Process.Enumerate


instantiateExpression
    :: MonadFail m
    => [(Name, Expression)]
    -> Expression
    -> m Constant
instantiateExpression ctxt x = normaliseConstant <$> evalStateT (instantiateE x) ctxt


instantiateDomain
    :: ( MonadFail m
       , Show r
       , Pretty r
       )
    => [(Name, Expression)]
    -> Domain r Expression
    -> m (Domain r Constant)
instantiateDomain ctxt x = normaliseDomain normaliseConstant <$> evalStateT (instantiateD x) ctxt


instantiateE
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       )
    => Expression
    -> m Constant

instantiateE (Comprehension body gensOrConds) = do
    let
        loop :: (MonadFail m, MonadState [(Name, Expression)] m) => [GeneratorOrCondition] -> m [Constant]
        loop [] = return <$> instantiateE body
        loop (Generator (GenDomainNoRepr pat domain) : rest) = do
            DomainInConstant domainConstant <- instantiateE (Domain domain)
            concatMapM
                (\ val -> scope $ bind pat val >> loop rest )
                (enumerateDomain domainConstant)
        loop (Generator (GenDomainHasRepr pat domain) : rest) = do
            DomainInConstant domainConstant <- instantiateE (Domain (forgetRepr "instantiateE" domain))
            concatMapM
                (\ val -> scope $ bind (Single pat) val >> loop rest )
                (enumerateDomain domainConstant)
        loop (Generator (GenInExpr pat expr) : rest) = do
            exprConstant <- instantiateE expr
            concatMapM
                (\ val -> scope $ bind pat val >> loop rest )
                (enumerateInConstant exprConstant)
        loop (Condition expr : rest) = do
            constant <- instantiateE expr
            if constant == ConstantBool True
                then loop rest
                else return []

    constants <- loop gensOrConds
    return $ ConstantAbstract $ AbsLitMatrix
        (DomainInt [RangeBounded 1 (fromInt (length constants))])
        constants

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
instantiateE (Typed x _) = instantiateE x       -- assuming we won't need a TypedConstant for now.
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


instantiateE x = fail $ "instantiateE:" <+> pretty (show x)


instantiateOp
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       )
    => Ops Expression
    -> m Constant
instantiateOp opx = mapM instantiateE opx >>= evaluateOp


instantiateAbsLit
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       )
    => AbstractLiteral Expression
    -> m (AbstractLiteral Constant)
instantiateAbsLit = mapM instantiateE


instantiateD
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       , Show r
       , Pretty r
       )
    => Domain r Expression
    -> m (Domain r Constant)
instantiateD DomainBool = return DomainBool
instantiateD (DomainInt ranges) = DomainInt <$> mapM instantiateR ranges
instantiateD (DomainEnum nm Nothing _) = do
    st <- gets id
    case lookup nm st of
        Just (Domain dom) -> instantiateD (anyRepr "instantiateD 1" dom)
        Just _  -> fail $ ("DomainEnum not found in state, Just:" <+> pretty nm) <++> vcat (map pretty st)
        Nothing -> fail $ ("DomainEnum not found in state, Nothing:" <+> pretty nm) <++> vcat (map pretty st)
instantiateD (DomainEnum nm rs _) = do
    st <- gets id
    mp <- forM (universeBi rs :: [Name]) $ \ n -> case lookup n st of
            Just (Constant (ConstantInt i)) -> return (n, i)
            Nothing -> fail $ "No value for member of enum domain:" <+> pretty n
            Just _  -> fail $ "Incompatible value for member of enum domain:" <+> pretty n
    return (DomainEnum nm rs (Just mp))
instantiateD (DomainUnnamed nm s) = DomainUnnamed nm <$> instantiateE s
instantiateD (DomainTuple inners) = DomainTuple <$> mapM instantiateD inners
instantiateD (DomainMatrix index inner) = DomainMatrix <$> instantiateD index <*> instantiateD inner
instantiateD (DomainSet       r attrs inner) = DomainSet r <$> instantiateSetAttr attrs <*> instantiateD inner
instantiateD (DomainMSet      r attrs inner) = DomainMSet r <$> instantiateMSetAttr attrs <*> instantiateD inner
instantiateD (DomainFunction  r attrs innerFr innerTo) = DomainFunction r <$> instantiateFunctionAttr attrs <*> instantiateD innerFr <*> instantiateD innerTo
instantiateD (DomainRelation  r attrs inners) = DomainRelation r <$> instantiateRelationAttr attrs <*> mapM instantiateD inners
instantiateD (DomainPartition r attrs inner) = DomainPartition r <$> instantiatePartitionAttr attrs <*> instantiateD inner
instantiateD (DomainOp {}) = bug "instantiateD DomainOp"
instantiateD (DomainReference _ (Just d)) = instantiateD d
instantiateD (DomainReference name Nothing) = do
    ctxt <- gets id
    case name `lookup` ctxt of
        Just (Domain d) -> instantiateD (anyRepr "instantiateD 2" d)
        _ -> fail $ vcat
            $ ("No value for:" <+> pretty name)
            : "Bindings in context:"
            : prettyContext ctxt
instantiateD DomainMetaVar{} = bug "instantiateD DomainMetaVar"


instantiateSetAttr
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       )
    => SetAttr Expression
    -> m (SetAttr Constant)
instantiateSetAttr (SetAttr s) = SetAttr <$> instantiateSizeAttr s


instantiateSizeAttr
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
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
       )
    => MSetAttr Expression
    -> m (MSetAttr Constant)
instantiateMSetAttr (MSetAttr s o) = MSetAttr <$> instantiateSizeAttr s <*> instantiateOccurAttr o


instantiateOccurAttr
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
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
       )
    => FunctionAttr Expression
    -> m (FunctionAttr Constant)
instantiateFunctionAttr (FunctionAttr s p j) =
    FunctionAttr <$> instantiateSizeAttr s
                 <*> pure p
                 <*> pure j


instantiateRelationAttr
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       )
    => RelationAttr Expression
    -> m (RelationAttr Constant)
instantiateRelationAttr (RelationAttr s b) = RelationAttr <$> instantiateSizeAttr s <*> pure b


instantiatePartitionAttr
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       )
    => PartitionAttr Expression
    -> m (PartitionAttr Constant)
instantiatePartitionAttr (PartitionAttr a b c d e) =
    PartitionAttr <$> instantiateSizeAttr a
                  <*> instantiateSizeAttr b
                  <*> instantiateSizeAttr c
                  <*> pure d
                  <*> pure e


instantiateR
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       )
    => Range Expression
    -> m (Range Constant)
instantiateR RangeOpen = return RangeOpen
instantiateR (RangeSingle x) = RangeSingle <$> instantiateE x
instantiateR (RangeLowerBounded x) = RangeLowerBounded <$> instantiateE x
instantiateR (RangeUpperBounded x) = RangeUpperBounded <$> instantiateE x
instantiateR (RangeBounded x y) = RangeBounded <$> instantiateE x <*> instantiateE y


bind :: MonadState [(Name, Expression)] m => AbstractPattern -> Constant -> m ()
bind (Single nm) val = modify ((nm, Constant val) :)
bind (AbsPatTuple  pats) (ConstantAbstract (AbsLitTuple    vals)) = zipWithM_ bind pats vals
bind (AbsPatMatrix pats) (ConstantAbstract (AbsLitMatrix _ vals)) = zipWithM_ bind pats vals
bind (AbsPatSet    pats) (ConstantAbstract (AbsLitSet      vals)) = zipWithM_ bind pats vals
bind pat val = bug $ "Instantiate.bind:" <++> vcat ["pat:" <+> pretty pat, "val:" <+> pretty val]
