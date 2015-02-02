{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.And where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpAnd x = OpAnd x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpAnd x)
instance Hashable  x => Hashable  (OpAnd x)
instance ToJSON    x => ToJSON    (OpAnd x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpAnd x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpAnd x) where
    opLexeme _ = L_And

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpAnd x) where
    typeOf p@(OpAnd x) = do
        ty <- typeOf x
        case ty of
            TypeList TypeBool -> return TypeBool
            TypeMatrix _ TypeBool -> return TypeBool
            _ -> raiseTypeError p

instance EvaluateOp OpAnd where
    evaluateOp (OpAnd x) = ConstantBool . and <$> boolsOut x

instance SimplifyOp OpAnd where
    simplifyOp _ _ = na "simplifyOp{OpAnd}"

instance (Pretty x, ExpressionLike x) => Pretty (OpAnd x) where
    prettyPrec prec op@(OpAnd x) | Just [a,b] <- listOut x = prettyPrecBinOp prec [op] a b
    prettyPrec _ (OpAnd x) = "and" <> prParens (pretty x)
