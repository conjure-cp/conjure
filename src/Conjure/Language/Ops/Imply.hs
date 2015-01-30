{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Imply where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpImply x = OpImply x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpImply x)
instance Hashable  x => Hashable  (OpImply x)
instance ToJSON    x => ToJSON    (OpImply x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpImply x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpImply x) where
    opLexeme _ = L_Imply

instance TypeOf x => TypeOf (OpImply x) where
    typeOf (OpImply a b) = boolToBoolToBool a b

instance EvaluateOp OpImply where
    evaluateOp (OpImply x y) = ConstantBool <$> ((<=) <$> boolOut x <*> boolOut y)

instance Pretty x => Pretty (OpImply x) where
    prettyPrec prec op@(OpImply a b) = prettyPrecBinOp prec [op] a b
