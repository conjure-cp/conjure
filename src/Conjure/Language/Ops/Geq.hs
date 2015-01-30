{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Geq where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpGeq x = OpGeq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpGeq x)
instance Hashable  x => Hashable  (OpGeq x)
instance ToJSON    x => ToJSON    (OpGeq x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpGeq x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpGeq x) where
    opLexeme _ = L_Geq

instance (TypeOf x, Pretty x) => TypeOf (OpGeq x) where
    typeOf (OpGeq a b) = sameToSameToBool a b

instance EvaluateOp OpGeq where
    evaluateOp (OpGeq x y) = return $ ConstantBool $ normaliseConstant x >= normaliseConstant y

instance Pretty x => Pretty (OpGeq x) where
    prettyPrec prec op@(OpGeq a b) = prettyPrecBinOp prec [op] a b
