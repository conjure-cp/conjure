{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Or where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpOr x = OpOr x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpOr x)
instance Hashable  x => Hashable  (OpOr x)
instance ToJSON    x => ToJSON    (OpOr x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpOr x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpOr x) where
    opLexeme _ = L_Or

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpOr x) where
    typeOf p@(OpOr x) = do
        ty <- typeOf x
        case ty of
            TypeList TypeBool -> return TypeBool
            TypeMatrix _ TypeBool -> return TypeBool
            _ -> raiseTypeError p

instance EvaluateOp OpOr where
    evaluateOp (OpOr x) = ConstantBool . or <$> boolsOut x

instance (Pretty x, ExpressionLike x) => Pretty (OpOr x) where
    prettyPrec prec op@(OpOr x) | Just [a,b] <- listOut x = prettyPrecBinOp prec [op] a b
    prettyPrec _ (OpOr x) = "or" <> prParens (pretty x)
