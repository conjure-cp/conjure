{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Min where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpMin x = OpMin [x]
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpMin x)
instance Hashable  x => Hashable  (OpMin x)
instance ToJSON    x => ToJSON    (OpMin x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMin x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpMin x) where
    typeOf (OpMin [a,b]) = intToIntToInt a b
    typeOf (OpMin [x]) = do
        TypeList TypeInt <- typeOf x
        return TypeInt
    typeOf p = raiseTypeError p

instance EvaluateOp OpMin where
    evaluateOp (OpMin [ConstantAbstract (AbsLitSet  xs)]) = ConstantInt . minimum <$> concatMapM intsOut xs
    evaluateOp (OpMin [ConstantAbstract (AbsLitMSet xs)]) = ConstantInt . minimum <$> concatMapM intsOut xs
    evaluateOp (OpMin                               xs)   = ConstantInt . minimum <$> concatMapM intsOut xs

instance Pretty x => Pretty (OpMin x) where
    prettyPrec _ (OpMin xs) = "min" <> prettyList prParens "," xs
