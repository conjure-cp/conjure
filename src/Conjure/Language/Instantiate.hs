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
import Conjure.Process.Enumerate


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

instantiateE (Comprehension body gensOrFilters) = do
    let
        loop :: (MonadFail m, MonadState [(Name, Expression)] m) => [GeneratorOrFilter] -> m [Constant]
        loop [] = return <$> instantiateE body
        loop (Generator (GenDomain pat domain) : rest) = do
            DomainInConstant domainConstant <- instantiateE (Domain domain)
            concatMapM
                (\ val -> scope $ bind pat val >> loop rest )
                (enumerateDomain domainConstant)
        loop (Generator (GenInExpr pat expr) : rest) = do
            exprConstant <- instantiateE expr
            concatMapM
                (\ val -> scope $ bind pat val >> loop rest )
                (enumerateInConstant exprConstant)
        loop (Filter expr : rest) = do
            constant <- instantiateE expr
            if constant == ConstantBool True
                then loop rest
                else return []

    constants <- loop gensOrFilters
    return $ ConstantAbstract $ AbsLitList constants

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


bind :: MonadState [(Name, Expression)] m => AbstractPattern -> Constant -> m ()
bind (Single nm) val = modify ((nm, Constant val) :)
bind (AbsPatTuple  pats) (ConstantAbstract (AbsLitTuple    vals)) = zipWithM_ bind pats vals
bind (AbsPatMatrix pats) (ConstantAbstract (AbsLitMatrix _ vals)) = zipWithM_ bind pats vals
bind pat val = bug $ "Instantiate.bind:" <++> vcat ["pat:" <+> pretty pat, "val:" <+> pretty val]
