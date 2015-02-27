{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Subset where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpSubset x = OpSubset x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSubset x)
instance Hashable  x => Hashable  (OpSubset x)
instance ToJSON    x => ToJSON    (OpSubset x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSubset x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpSubset x) where
    opLexeme _ = L_subset

instance (TypeOf x, Pretty x) => TypeOf (OpSubset x) where
    typeOf (OpSubset a b) = sameToSameToBool a b

instance Pretty x => DomainOf (OpSubset x) x where
    domainOf op = na $ "evaluateOp{OpSubset}:" <++> pretty op

instance EvaluateOp OpSubset where
    evaluateOp (OpSubset (ConstantAbstract (AbsLitSet as)) (ConstantAbstract (AbsLitSet bs))) =
        return $ ConstantBool $ all (`elem` bs) as && length as <= length bs
    evaluateOp (OpSubset (ConstantAbstract (AbsLitMSet as)) (ConstantAbstract (AbsLitMSet bs))) =
        let asHist = histogram as
            bsHist = histogram bs
            allElems = sortNub (as++bs)
        in return $ ConstantBool $ and
            [ and
                [ countA <= countB
                | e <- allElems
                , let countA = fromMaybe 0 (e `lookup` asHist)
                , let countB = fromMaybe 0 (e `lookup` bsHist)
                ]
            , or
                [ countA < countB
                | e <- allElems
                , let countA = fromMaybe 0 (e `lookup` asHist)
                , let countB = fromMaybe 0 (e `lookup` bsHist)
                ]
            ]
    evaluateOp op = na $ "evaluateOp{OpSubset}:" <++> pretty (show op)

instance SimplifyOp OpSubset where
    simplifyOp _ _ = na "simplifyOp{OpSubset}"

instance Pretty x => Pretty (OpSubset x) where
    prettyPrec prec op@(OpSubset a b) = prettyPrecBinOp prec [op] a b
