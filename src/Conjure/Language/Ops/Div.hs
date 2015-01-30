{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Div where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpDiv x = OpDiv x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpDiv x)
instance Hashable  x => Hashable  (OpDiv x)
instance ToJSON    x => ToJSON    (OpDiv x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDiv x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpDiv x) where
    opLexeme _ = L_Div

instance TypeOf x => TypeOf (OpDiv x) where
    typeOf (OpDiv a b) = intToIntToInt a b

instance EvaluateOp OpDiv where
    evaluateOp (OpDiv x y) = ConstantInt <$> (div <$> intOut x <*> intOut y)

instance Pretty x => Pretty (OpDiv x) where
    prettyPrec prec op@(OpDiv a b) = prettyPrecBinOp prec [op] a b
