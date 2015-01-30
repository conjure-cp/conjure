{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Pow where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpPow x = OpPow x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpPow x)
instance Hashable  x => Hashable  (OpPow x)
instance ToJSON    x => ToJSON    (OpPow x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpPow x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpPow x) where
    opLexeme _ = L_Pow

instance TypeOf x => TypeOf (OpPow x) where
    typeOf (OpPow a b) = intToIntToInt a b

instance EvaluateOp OpPow where
    evaluateOp (OpPow x y) = ConstantInt <$> ((^) <$> intOut x <*> intOut y)

instance Pretty x => Pretty (OpPow x) where
    prettyPrec prec op@(OpPow a b) = prettyPrecBinOp prec [op] a b
