{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Compute.DomainOf where

-- conjure
import Conjure.Prelude
import Conjure.Bug

import Conjure.Language

import Conjure.Language.Pretty
import Conjure.Language.TypeOf
import Conjure.Compute.DomainUnion

type Dom = Domain () Expression

class DomainOf a where
    domainOf
        :: MonadFail m
        => a -> m Dom


instance DomainOf ReferenceTo where
    domainOf (Alias x) = domainOf x
    domainOf InComprehension{} = fail "domainOf-ReferenceTo-InComprehension"
    domainOf (DeclNoRepr  _ _ dom) = return dom
    domainOf (DeclHasRepr _ _ dom) = return (forgetRepr dom)
    domainOf RecordField{}  = fail "domainOf-ReferenceTo-RecordField"
    domainOf VariantField{} = fail "domainOf-ReferenceTo-VariantField"


instance DomainOf Expression where
    domainOf (Reference _ (Just refTo)) = domainOf refTo
    domainOf (Constant x) = domainOf x
    domainOf (AbstractLiteral x) = domainOf x
    domainOf (Op x) = domainOf x
    domainOf x = fail ("domainOf{Expression} 1:" <+> pretty (show x))

-- this should be better implemented by some ghc-generics magic
instance (DomainOf x, TypeOf x, Pretty x, ExpressionLike x, Domain () x :< x, Dom :< x) => DomainOf (Op x) where
    domainOf (MkOpActive x) = domainOf x
    domainOf (MkOpAllDiff x) = domainOf x
    domainOf (MkOpAnd x) = domainOf x
    domainOf (MkOpApart x) = domainOf x
    domainOf (MkOpAttributeAsConstraint x) = domainOf x
    domainOf (MkOpDefined x) = domainOf x
    domainOf (MkOpDiv x) = domainOf x
    domainOf (MkOpDontCare x) = domainOf x
    domainOf (MkOpDotLeq x) = domainOf x
    domainOf (MkOpDotLt x) = domainOf x
    domainOf (MkOpEq x) = domainOf x
    domainOf (MkOpFactorial x) = domainOf x
    domainOf (MkOpFlatten x) = domainOf x
    domainOf (MkOpFreq x) = domainOf x
    domainOf (MkOpGeq x) = domainOf x
    domainOf (MkOpGt x) = domainOf x
    domainOf (MkOpHist x) = domainOf x
    domainOf (MkOpIff x) = domainOf x
    domainOf (MkOpImage x) = domainOf x
    domainOf (MkOpImageSet x) = domainOf x
    domainOf (MkOpImply x) = domainOf x
    domainOf (MkOpIn x) = domainOf x
    domainOf (MkOpIndexing x) = domainOf x
    domainOf (MkOpIntersect x) = domainOf x
    domainOf (MkOpInverse x) = domainOf x
    domainOf (MkOpLeq x) = domainOf x
    domainOf (MkOpLexLeq x) = domainOf x
    domainOf (MkOpLexLt x) = domainOf x
    domainOf (MkOpLt x) = domainOf x
    domainOf (MkOpMax x) = domainOf x
    domainOf (MkOpMin x) = domainOf x
    domainOf (MkOpMinus x) = domainOf x
    domainOf (MkOpMod x) = domainOf x
    domainOf (MkOpNegate x) = domainOf x
    domainOf (MkOpNeq x) = domainOf x
    domainOf (MkOpNot x) = domainOf x
    domainOf (MkOpOr x) = domainOf x
    domainOf (MkOpParticipants x) = domainOf x
    domainOf (MkOpParts x) = domainOf x
    domainOf (MkOpParty x) = domainOf x
    domainOf (MkOpPow x) = domainOf x
    domainOf (MkOpPowerSet x) = domainOf x
    domainOf (MkOpPreImage x) = domainOf x
    domainOf (MkOpPred x) = domainOf x
    domainOf (MkOpProduct x) = domainOf x
    domainOf (MkOpRange x) = domainOf x
    domainOf (MkOpRelationProj x) = domainOf x
    domainOf (MkOpRestrict x) = domainOf x
    domainOf (MkOpSlicing x) = domainOf x
    domainOf (MkOpSubsequence x) = domainOf x
    domainOf (MkOpSubset x) = domainOf x
    domainOf (MkOpSubsetEq x) = domainOf x
    domainOf (MkOpSubstring x) = domainOf x
    domainOf (MkOpSucc x) = domainOf x
    domainOf (MkOpSum x) = domainOf x
    domainOf (MkOpSupset x) = domainOf x
    domainOf (MkOpSupsetEq x) = domainOf x
    domainOf (MkOpTildeLeq x) = domainOf x
    domainOf (MkOpTildeLt x) = domainOf x
    domainOf (MkOpToInt x) = domainOf x
    domainOf (MkOpToMSet x) = domainOf x
    domainOf (MkOpToRelation x) = domainOf x
    domainOf (MkOpToSet x) = domainOf x
    domainOf (MkOpTogether x) = domainOf x
    domainOf (MkOpTrue x) = domainOf x
    domainOf (MkOpTwoBars x) = domainOf x
    domainOf (MkOpUnion x) = domainOf x

instance DomainOf Constant where
    domainOf ConstantBool{}             = return DomainBool
    domainOf i@ConstantInt{}            = return $ DomainInt [RangeSingle (Constant i)]
    domainOf (ConstantEnum defn _ _ )   = return (DomainEnum defn Nothing Nothing)
    domainOf ConstantField{}            = fail "DomainOf-Constant-ConstantField"
    domainOf (ConstantAbstract x)       = domainOf (fmap Constant x)
    domainOf (DomainInConstant dom)     = return (fmap Constant dom)
    domainOf (TypedConstant x _)        = domainOf x
    domainOf ConstantUndefined{}        = fail "DomainOf-Constant-ConstantUndefined"


instance DomainOf (AbstractLiteral Expression) where

    domainOf (AbsLitTuple        xs) = DomainTuple  <$> mapM domainOf xs

    domainOf (AbsLitRecord       xs) = DomainRecord <$> sequence [ do t <- domainOf x ; return (n,t)
                                                                 | (n,x) <- xs ]

    domainOf (AbsLitVariant Nothing  _ _) = fail "Cannot calculate the domain of variant literal."
    domainOf (AbsLitVariant (Just t) _ _) = return (DomainVariant t)

    domainOf (AbsLitMatrix ind inn ) = DomainMatrix ind <$> (domainUnions =<< mapM domainOf inn)

    domainOf (AbsLitSet         xs ) = DomainSet def def <$> (domainUnions =<< mapM domainOf xs)

    domainOf (AbsLitMSet        xs ) = DomainMSet def def <$> (domainUnions =<< mapM domainOf xs)

    domainOf (AbsLitFunction    xs ) = DomainFunction def def
                                                <$> (domainUnions =<< mapM (domainOf . fst) xs)
                                                <*> (domainUnions =<< mapM (domainOf . snd) xs)

    domainOf (AbsLitSequence    xs ) =
        let
            attr = SequenceAttr (SizeAttr_Size (fromInt $ genericLength xs)) def
        in
            DomainSequence def attr <$> (domainUnions =<< mapM domainOf xs)

    domainOf (AbsLitRelation    xss) = do
        ty <- domainUnions =<< mapM (domainOf . AbsLitTuple) xss
        case ty of
            DomainTuple ts -> return (DomainRelation def def ts)
            _ -> bug "expecting DomainTuple in domainOf"

    domainOf (AbsLitPartition   xss) = DomainPartition def def <$> (domainUnions =<< mapM domainOf (concat xss))




-- all the `Op`s

instance (Pretty x, TypeOf x) => DomainOf (OpActive x) where
    domainOf op = mkDomainAny ("OpActive:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpAllDiff x) where
    domainOf op = mkDomainAny ("OpAllDiff:" <++> pretty op) <$> typeOf op

instance (Pretty x, ExpressionLike x, TypeOf x) => DomainOf (OpAnd x) where
    domainOf op = mkDomainAny ("OpAnd:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpApart x) where
    domainOf op = mkDomainAny ("OpApart:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpAttributeAsConstraint x) where
    domainOf op = mkDomainAny ("OpAttributeAsConstraint:" <++> pretty op) <$> typeOf op

instance (Pretty x, DomainOf x) => DomainOf (OpDefined x) where
    domainOf (OpDefined f) = do
        fDom <- domainOf f
        case fDom of
            DomainFunction _ _ fr _ -> return $ DomainSet def def fr
            _ -> fail "domainOf, OpDefined, not a function"

instance (Pretty x, TypeOf x) => DomainOf (OpDiv x) where
    domainOf op = mkDomainAny ("OpDiv:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpDontCare x) where
    domainOf op = mkDomainAny ("OpDontCare:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpDotLeq x) where
    domainOf op = mkDomainAny ("OpDotLeq:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpDotLt x) where
    domainOf op = mkDomainAny ("OpDotLt:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpEq x) where
    domainOf op = mkDomainAny ("OpEq:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpFactorial x) where
    domainOf op = mkDomainAny ("OpFactorial:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpFlatten x) where
    domainOf op = mkDomainAny ("OpFlatten:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpFreq x) where
    domainOf op = mkDomainAny ("OpFreq:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpGeq x) where
    domainOf op = mkDomainAny ("OpGeq:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpGt x) where
    domainOf op = mkDomainAny ("OpGt:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpHist x) where
    domainOf op = mkDomainAny ("OpHist:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpIff x) where
    domainOf op = mkDomainAny ("OpIff:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpImage x) where
    domainOf op = mkDomainAny ("OpImage:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpImageSet x) where
    domainOf op = mkDomainAny ("OpImageSet:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpImply x) where
    domainOf op = mkDomainAny ("OpImply:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpIn x) where
    domainOf op = mkDomainAny ("OpIn:" <++> pretty op) <$> typeOf op

instance (Pretty x, ExpressionLike x, DomainOf x, TypeOf x) => DomainOf (OpIndexing x) where
    domainOf (OpIndexing m i) = do
        iType <- typeOf i
        case iType of
            TypeBool{} -> return ()
            TypeInt{} -> return ()
            _ -> fail "domainOf, OpIndexing, not a bool or int index"
        mDom <- domainOf m
        case mDom of
            DomainMatrix _ inner -> return inner
            DomainTuple inners -> do
                iInt <- intOut i
                return $ atNote "domainOf" inners (fromInteger (iInt-1))
            _ -> fail "domainOf, OpIndexing, not a matrix or tuple"

instance (Pretty x, TypeOf x) => DomainOf (OpIntersect x) where
    domainOf op = mkDomainAny ("OpIntersect:" <++> pretty op) <$> typeOf op

instance DomainOf (OpInverse x) where
    domainOf _ = return DomainBool

instance (Pretty x, TypeOf x) => DomainOf (OpLeq x) where
    domainOf op = mkDomainAny ("OpLeq:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpLexLeq x) where
    domainOf op = mkDomainAny ("OpLexLeq:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpLexLt x) where
    domainOf op = mkDomainAny ("OpLexLt:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpLt x) where
    domainOf op = mkDomainAny ("OpLt:" <++> pretty op) <$> typeOf op

instance ( TypeOf x, Pretty x, ExpressionLike x
         , Domain () x :< x
         ) => DomainOf (OpMax x) where
    domainOf op = mkDomainAny ("OpMax:" <++> pretty op) <$> typeOf op

instance ( TypeOf x, Pretty x, ExpressionLike x
         , Domain () x :< x
         ) => DomainOf (OpMin x) where
    domainOf op = mkDomainAny ("OpMin:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x, DomainOf x) => DomainOf (OpMinus x) where
    domainOf (OpMinus x y) = do
        xDom :: Dom <- domainOf x
        yDom :: Dom <- domainOf y
        let l = [essence| min(`&xDom`) - max(`&yDom`) |]
        let u = [essence| max(`&xDom`) - min(`&yDom`) |]
        return (DomainInt [RangeBounded l u] :: Dom)

instance (Pretty x, TypeOf x) => DomainOf (OpMod x) where
    domainOf op = mkDomainAny ("OpMod:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpNegate x) where
    domainOf op = mkDomainAny ("OpNegate:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpNeq x) where
    domainOf op = mkDomainAny ("OpNeq:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpNot x) where
    domainOf op = mkDomainAny ("OpNot:" <++> pretty op) <$> typeOf op

instance (Pretty x, ExpressionLike x, TypeOf x) => DomainOf (OpOr x) where
    domainOf op = mkDomainAny ("OpOr:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpParticipants x) where
    domainOf op = mkDomainAny ("OpParticipants:" <++> pretty op) <$> typeOf op

instance (Pretty x, DomainOf x) => DomainOf (OpParts x) where
    domainOf (OpParts p) = do
        dom <- domainOf p
        case dom of
            DomainPartition _ _ inner -> return $ DomainSet def def $ DomainSet def def inner
            _ -> fail "domainOf, OpParts, not a partition"

instance (Pretty x, TypeOf x) => DomainOf (OpParty x) where
    domainOf op = mkDomainAny ("OpParty:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpPow x) where
    domainOf op = mkDomainAny ("OpPow:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpPowerSet x) where
    domainOf op = mkDomainAny ("OpPowerSet:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpPreImage x) where
    domainOf op = mkDomainAny ("OpPreImage:" <++> pretty op) <$> typeOf op

instance (DomainOf x) => DomainOf (OpPred x) where
    domainOf (OpPred x) = domainOf x

instance (Pretty x, ExpressionLike x, TypeOf x) => DomainOf (OpProduct x) where
    domainOf op = mkDomainAny ("OpProduct:" <++> pretty op) <$> typeOf op

instance (Pretty x, DomainOf x) => DomainOf (OpRange x) where
    domainOf (OpRange f) = do
        fDom <- domainOf f
        case fDom of
            DomainFunction _ _ _ to -> return $ DomainSet def def to
            _ -> fail "domainOf, OpRange, not a function"

instance (Pretty x, TypeOf x) => DomainOf (OpRelationProj x) where
    domainOf op = mkDomainAny ("OpRelationProj:" <++> pretty op) <$> typeOf op

instance (Pretty x, DomainOf x, Dom :< x) => DomainOf (OpRestrict x) where
    domainOf (OpRestrict f x) = do
        d    <- project x
        fDom <- domainOf f
        case fDom of
            DomainFunction fRepr a _ to -> return (DomainFunction fRepr a d to)
            _ -> fail "domainOf, OpRestrict, not a function"

instance (Pretty x, TypeOf x) => DomainOf (OpSlicing x) where
    domainOf op = mkDomainAny ("OpSlicing:" <++> pretty op) <$> typeOf op

instance DomainOf (OpSubsequence x) where
    domainOf _ = fail "domainOf{OpSubsequence}"

instance (Pretty x, TypeOf x) => DomainOf (OpSubset x) where
    domainOf op = mkDomainAny ("OpSubset:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpSubsetEq x) where
    domainOf op = mkDomainAny ("OpSubsetEq:" <++> pretty op) <$> typeOf op

instance DomainOf (OpSubstring x) where
    domainOf _ = fail "domainOf{OpSubstring}"

instance (DomainOf x) => DomainOf (OpSucc x) where
    domainOf (OpSucc x) = domainOf x

instance (Pretty x, ExpressionLike x, TypeOf x) => DomainOf (OpSum x) where
    domainOf op = mkDomainAny ("OpSum:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpSupset x) where
    domainOf op = mkDomainAny ("OpSupset:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpSupsetEq x) where
    domainOf op = mkDomainAny ("OpSupsetEq:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpTildeLeq x) where
    domainOf op = mkDomainAny ("OpTildeLeq:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpTildeLt x) where
    domainOf op = mkDomainAny ("OpTildeLt:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpToInt x) where
    domainOf op = mkDomainAny ("OpToInt:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpToMSet x) where
    domainOf op = mkDomainAny ("OpToMSet:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpToRelation x) where
    domainOf op = mkDomainAny ("OpToRelation:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpToSet x) where
    domainOf op = mkDomainAny ("OpToSet:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpTogether x) where
    domainOf op = mkDomainAny ("OpTogether:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpTrue x) where
    domainOf op = mkDomainAny ("OpTrue:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpTwoBars x) where
    domainOf op = mkDomainAny ("OpTwoBars:" <++> pretty op) <$> typeOf op

instance (Pretty x, TypeOf x) => DomainOf (OpUnion x) where
    domainOf op = mkDomainAny ("OpUnion:" <++> pretty op) <$> typeOf op

