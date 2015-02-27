{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.SubsetEq where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpSubsetEq x = OpSubsetEq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSubsetEq x)
instance Hashable  x => Hashable  (OpSubsetEq x)
instance ToJSON    x => ToJSON    (OpSubsetEq x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSubsetEq x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpSubsetEq x) where
    opLexeme _ = L_subsetEq

instance (TypeOf x, Pretty x) => TypeOf (OpSubsetEq x) where
    typeOf (OpSubsetEq a b) = sameToSameToBool a b

instance Pretty x => DomainOf (OpSubsetEq x) x where
    domainOf op = na $ "evaluateOp{OpSubsetEq}:" <++> pretty op

instance EvaluateOp OpSubsetEq where
    evaluateOp (OpSubsetEq (ConstantAbstract (AbsLitSet as)) (ConstantAbstract (AbsLitSet bs))) =
        return $ ConstantBool $ all (`elem` bs) as
    evaluateOp (OpSubsetEq (ConstantAbstract (AbsLitMSet as)) (ConstantAbstract (AbsLitMSet bs))) =
        let asHist = histogram as
            bsHist = histogram bs
            allElems = sortNub (as++bs)
        in return $ ConstantBool $ and
            [ countA <= countB
            | e <- allElems
            , let countA = fromMaybe 0 (e `lookup` asHist)
            , let countB = fromMaybe 0 (e `lookup` bsHist)
            ]
    evaluateOp op = na $ "evaluateOp{OpSubsetEq}:" <++> pretty (show op)

instance SimplifyOp OpSubsetEq where
    simplifyOp _ _ = na "simplifyOp{OpSubsetEq}"

instance Pretty x => Pretty (OpSubsetEq x) where
    prettyPrec prec op@(OpSubsetEq a b) = prettyPrecBinOp prec [op] a b
