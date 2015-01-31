{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Eq where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpEq x = OpEq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpEq x)
instance Hashable  x => Hashable  (OpEq x)
instance ToJSON    x => ToJSON    (OpEq x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpEq x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpEq x) where
    opLexeme _ = L_Eq

instance (TypeOf x, Pretty x) => TypeOf (OpEq x) where
    typeOf (OpEq a b) = sameToSameToBool a b

instance EvaluateOp OpEq where
    evaluateOp (OpEq x y) = return $ ConstantBool $ x == y

instance Pretty x => Pretty (OpEq x) where
    prettyPrec prec op@(OpEq a b) = prettyPrecBinOp prec [op] a b
