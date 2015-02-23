{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Intersect where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpIntersect x = OpIntersect x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpIntersect x)
instance Hashable  x => Hashable  (OpIntersect x)
instance ToJSON    x => ToJSON    (OpIntersect x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpIntersect x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpIntersect x) where
    opLexeme _ = L_intersect

instance (TypeOf x, Pretty x) => TypeOf (OpIntersect x) where
    typeOf (OpIntersect a b) = sameToSameToSame a b

instance EvaluateOp OpIntersect where
    evaluateOp p@(OpIntersect (ConstantAbstract (AbsLitSet as)) (ConstantAbstract (AbsLitSet bs))) = do
        ty <- typeOf p
        let outs = sortNub [ i | i <- as, i `elem` bs]
        return $ if null outs
            then TypedConstant (ConstantAbstract (AbsLitSet [])) ty
            else ConstantAbstract $ AbsLitSet outs
    evaluateOp p@(OpIntersect (ConstantAbstract (AbsLitMSet as)) (ConstantAbstract (AbsLitMSet bs))) = do
        ty <- typeOf p
        let asHist = histogram as
            bsHist = histogram bs
            allElems = sortNub (as++bs)
            outs =
                [ replicate (min countA countB) e
                | e <- allElems
                , let countA = fromMaybe 0 (e `lookup` asHist)
                , let countB = fromMaybe 0 (e `lookup` bsHist)
                ]
        return $ if null outs
            then TypedConstant (ConstantAbstract (AbsLitMSet [])) ty
            else ConstantAbstract $ AbsLitMSet $ concat outs
    -- TODO: what if the same thing is mapped to two different values? undefined behaviour?
    evaluateOp p@(OpIntersect (ConstantAbstract (AbsLitFunction as)) (ConstantAbstract (AbsLitFunction bs))) = do
        ty <- typeOf p
        let outs = sortNub [ i | i <- as, i `elem` bs]
        return $ if null outs
            then TypedConstant (ConstantAbstract (AbsLitFunction [])) ty
            else ConstantAbstract $ AbsLitFunction outs
    evaluateOp p@(OpIntersect (ConstantAbstract (AbsLitRelation as)) (ConstantAbstract (AbsLitRelation bs))) = do
        ty <- typeOf p
        let outs = sortNub [ i | i <- as, i `elem` bs]
        return $ if null outs
            then TypedConstant (ConstantAbstract (AbsLitRelation [])) ty
            else ConstantAbstract $ AbsLitRelation outs
    evaluateOp op = na $ "evaluateOp{OpIntersect}:" <++> pretty (show op)

instance SimplifyOp OpIntersect where
    simplifyOp _ _ = na "simplifyOp{OpIntersect}"

instance Pretty x => Pretty (OpIntersect x) where
    prettyPrec prec op@(OpIntersect a b) = prettyPrecBinOp prec [op] a b
