{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Iff where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpIff x = OpIff x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpIff x)
instance Hashable  x => Hashable  (OpIff x)
instance ToJSON    x => ToJSON    (OpIff x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpIff x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpIff x) where
    opLexeme _ = L_Iff

instance (TypeOf x, Pretty x) => TypeOf (OpIff x) where
    typeOf (OpIff a b) = boolToBoolToBool a b

instance EvaluateOp OpIff where
    evaluateOp (OpIff (ConstantBool x) (ConstantBool y)) = return $ ConstantBool $ x == y
    evaluateOp _ = na "evaluateOp{OpIff}"

instance SimplifyOp OpIff where
    simplifyOp _ (OpIff a b)
        | fromBool True == a = return b
        | fromBool True == b = return a
    simplifyOp _ _ = na "simplifyOp{OpIff}"

instance Pretty x => Pretty (OpIff x) where
    prettyPrec prec op@(OpIff a b) = prettyPrecBinOp prec [op] a b
