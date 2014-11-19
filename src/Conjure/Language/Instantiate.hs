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
import Conjure.Language.Pretty
import Conjure.Process.EnumerateDomain ( enumerateDomain )


instantiateExpression
    :: MonadFail m
    => [(Name, Expression)]
    -> Expression
    -> m Constant
instantiateExpression ctxt x = evalStateT (instantiateE x) ctxt


instantiateDomain
    :: ( MonadFail m
       , Show r
       )
    => [(Name, Expression)]
    -> Domain r Expression
    -> m (Domain r Constant)
instantiateDomain ctxt x = evalStateT (instantiateD x) ctxt


instantiateE
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       )
    => Expression
    -> m Constant

instantiateE (Comprehension l2 [Generator (GenDomain l1 domain)]) = do
    let l = lambdaToFunction l1 l2
    constantDomain <- instantiateE (Domain domain)
    case constantDomain of
        DomainInConstant domain' -> do
            let valuesInDomain =  enumerateDomain domain'
            let intDomain      =  DomainInt [RangeBounded (fromInt 1) (fromInt (length valuesInDomain))]
            constantsInMatrix  <- mapM (instantiateE . l . Constant) valuesInDomain
            return $ ConstantAbstract $ AbsLitMatrix intDomain constantsInMatrix
        _ -> bug ("instantiateE MkOpMapOverDomain:" <++> pretty (show constantDomain))

instantiateE (Comprehension l2 [Generator (GenInExpr l1 expr)]) = do
    let l = lambdaToFunction l1 l2
    constant          <- instantiateE expr
    constantsInMatrix <- case constant of
        ConstantAbstract (AbsLitSet      xs) -> mapM (instantiateE . l . Constant                                 ) xs
        ConstantAbstract (AbsLitRelation xs) -> mapM (instantiateE . l . Constant . ConstantAbstract . AbsLitTuple) xs
        _ -> bug $ vcat [ "instantiateE"
                        , "l1      :" <+> pretty l1
                        , "l2      :" <+> pretty l2
                        , "expr    :" <+> pretty expr
                        , "constant:" <+> pretty constant
                        ]
    return $ ConstantAbstract $ AbsLitMatrix
                (DomainInt [RangeBounded (fromInt 1) (fromInt (length constantsInMatrix))])
                constantsInMatrix

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
       )
    => Domain r Expression
    -> m (Domain r Constant)
instantiateD DomainBool = return DomainBool
instantiateD (DomainInt ranges) = DomainInt <$> mapM instantiateR ranges
instantiateD (DomainEnum nm rs) = return (DomainEnum nm rs)
instantiateD (DomainUnnamed nm s) = DomainUnnamed nm <$> instantiateE s
instantiateD (DomainTuple inners) = DomainTuple <$> mapM instantiateD inners
instantiateD (DomainMatrix index inner) = DomainMatrix <$> instantiateD index <*> instantiateD inner
instantiateD (DomainSet       r attrs inner) = DomainSet r <$> instantiateSetAttr attrs <*> instantiateD inner
instantiateD (DomainMSet      r attrs inner) = DomainMSet r <$> instantiateDAs attrs <*> instantiateD inner
instantiateD (DomainFunction  r attrs innerFr innerTo) = DomainFunction r <$> instantiateFunctionAttr attrs <*> instantiateD innerFr <*> instantiateD innerTo
instantiateD (DomainRelation  r attrs inners) = DomainRelation r <$> instantiateRelationAttr attrs <*> mapM instantiateD inners
instantiateD (DomainPartition r attrs inner) = DomainPartition r <$> instantiateDAs attrs <*> instantiateD inner
instantiateD (DomainOp {}) = bug "instantiateD DomainOp"
instantiateD (DomainReference _ (Just d)) = instantiateD d
instantiateD (DomainReference nm Nothing) = gets id >>= \ ctxt ->
                                               fail $ vcat $ ("Undefined domain reference:" <+> pretty nm)
                                                    : ("Bindings in context:" : prettyContext ctxt)
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
instantiateSizeAttr SizeAttrNone = return SizeAttrNone
instantiateSizeAttr (SizeAttrSize x) = SizeAttrSize <$> instantiateE x
instantiateSizeAttr (SizeAttrMinSize x) = SizeAttrMinSize <$> instantiateE x
instantiateSizeAttr (SizeAttrMaxSize x) = SizeAttrMaxSize <$> instantiateE x
instantiateSizeAttr (SizeAttrMinMaxSize x y) = SizeAttrMinMaxSize <$> instantiateE x <*> instantiateE y


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
instantiateRelationAttr (RelationAttr s) = RelationAttr <$> instantiateSizeAttr s


instantiateDAs
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       )
    => DomainAttributes Expression
    -> m (DomainAttributes Constant)
instantiateDAs (DomainAttributes xs) = DomainAttributes <$> mapM instantiateDA xs


instantiateDA
    :: ( MonadFail m
       , MonadState [(Name, Expression)] m
       )
    => DomainAttribute Expression
    -> m (DomainAttribute Constant)
instantiateDA (DAName n) = return (DAName n)
instantiateDA (DANameValue n x) = DANameValue n <$> instantiateE x
instantiateDA DADotDot = return DADotDot


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

