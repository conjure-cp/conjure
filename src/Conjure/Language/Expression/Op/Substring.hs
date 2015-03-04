{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Substring where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpSubstring x = OpSubstring x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSubstring x)
instance Hashable  x => Hashable  (OpSubstring x)
instance ToJSON    x => ToJSON    (OpSubstring x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSubstring x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpSubstring x) where
    opLexeme _ = L_substring

instance (TypeOf x, Pretty x) => TypeOf (OpSubstring x) where
    typeOf p@(OpSubstring a b) = do
        tya <- typeOf a
        tyb <- typeOf b
        case (tya, tyb) of
            (TypeSequence{}, TypeSequence{}) -> return TypeBool
            _ -> raiseTypeError p

instance DomainOf (OpSubstring x) x where
    domainOf _ = fail "domainOf{OpSubstring}"

instance EvaluateOp OpSubstring where
    evaluateOp op = na $ "evaluateOp{OpSubstring}:" <++> pretty (show op)

instance SimplifyOp OpSubstring x where
    simplifyOp _ = na "simplifyOp{OpSubstring}"

instance Pretty x => Pretty (OpSubstring x) where
    prettyPrec prec op@(OpSubstring a b) = prettyPrecBinOp prec [op] a b
