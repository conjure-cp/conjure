{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Sum where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpSum x = OpSum x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSum x)
instance Hashable  x => Hashable  (OpSum x)
instance ToJSON    x => ToJSON    (OpSum x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSum x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpSum x) where
    typeOf p@(OpSum x) = do
        ty <- typeOf x
        case ty of
            TypeList TypeInt -> return TypeInt
            TypeMatrix _ TypeInt -> return TypeInt
            _ -> raiseTypeError p

instance BinaryOperator (OpSum x) where
    opLexeme _ = L_Plus

instance EvaluateOp OpSum where
    evaluateOp (OpSum x) = ConstantInt . sum <$> intsOut x

instance (Pretty x, ExpressionLike x) => Pretty (OpSum x) where
    prettyPrec prec op@(OpSum x) | Just [a,b] <- listOut x = prettyPrecBinOp prec [op] a b
    prettyPrec _ (OpSum x) = "sum" <> prParens (pretty x)
