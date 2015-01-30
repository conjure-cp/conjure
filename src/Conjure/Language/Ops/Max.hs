{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Max where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpMax x = OpMax [x]
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpMax x)
instance Hashable  x => Hashable  (OpMax x)
instance ToJSON    x => ToJSON    (OpMax x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMax x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpMax x) where
    typeOf (OpMax [a,b]) = intToIntToInt a b
    typeOf (OpMax [x]) = do
        TypeList TypeInt <- typeOf x
        return TypeInt
    typeOf p = raiseTypeError p

instance EvaluateOp OpMax where
    evaluateOp (OpMax [ConstantAbstract (AbsLitSet  xs)]) = ConstantInt . maximum <$> concatMapM intsOut xs
    evaluateOp (OpMax [ConstantAbstract (AbsLitMSet xs)]) = ConstantInt . maximum <$> concatMapM intsOut xs
    evaluateOp (OpMax                               xs)   = ConstantInt . maximum <$> concatMapM intsOut xs

instance Pretty x => Pretty (OpMax x) where
    prettyPrec _ (OpMax xs) = "max" <> prettyList prParens "," xs
