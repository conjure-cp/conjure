{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Union where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpUnion x = OpUnion x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpUnion x)
instance Hashable  x => Hashable  (OpUnion x)
instance ToJSON    x => ToJSON    (OpUnion x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpUnion x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpUnion x) where
    opLexeme _ = L_union

instance (TypeOf x, Pretty x) => TypeOf (OpUnion x) where
    typeOf (OpUnion a b) = sameToSameToSame a b

instance EvaluateOp OpUnion where
    evaluateOp (OpUnion (ConstantAbstract (AbsLitSet as)) (ConstantAbstract (AbsLitSet bs))) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub (as ++ bs)
    evaluateOp (OpUnion (ConstantAbstract (AbsLitMSet as)) (ConstantAbstract (AbsLitMSet bs))) =
        let asHist = histogram as
            bsHist = histogram bs
            allElems = sortNub (as++bs)
        in
            return $ ConstantAbstract $ AbsLitMSet $ concat
                [ replicate (fromInteger (max countA countB)) e
                | e <- allElems
                , let countA = fromMaybe 0 (e `lookup` asHist)
                , let countB = fromMaybe 0 (e `lookup` bsHist)
                ]
    -- TODO: what if the same thing is mapped to two different values? undefined behaviour?
    evaluateOp (OpUnion (ConstantAbstract (AbsLitFunction as)) (ConstantAbstract (AbsLitFunction bs))) =
        return $ ConstantAbstract $ AbsLitFunction $ sortNub (as ++ bs)
    evaluateOp (OpUnion (ConstantAbstract (AbsLitRelation as)) (ConstantAbstract (AbsLitRelation bs))) =
        return $ ConstantAbstract $ AbsLitRelation $ sortNub (as ++ bs)
    evaluateOp op = na $ "evaluateOp{OpUnion}:" <++> pretty (show op)

instance SimplifyOp OpUnion where
    simplifyOp _ _ = na "simplifyOp{OpUnion}"

instance Pretty x => Pretty (OpUnion x) where
    prettyPrec prec op@(OpUnion a b) = prettyPrecBinOp prec [op] a b
