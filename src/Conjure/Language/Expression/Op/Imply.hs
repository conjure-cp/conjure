{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Imply where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


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

instance Pretty x => DomainOf (OpImply x) x where
    domainOf op = na $ "evaluateOp{OpImply}:" <++> pretty op

instance EvaluateOp OpImply where
    evaluateOp (OpImply x y) = ConstantBool <$> ((<=) <$> boolOut x <*> boolOut y)

instance SimplifyOp OpImply x where
    simplifyOp (OpImply a b)
        | fromBool True  == a = return b
        | fromBool False == a = return $ fromBool True
        | fromBool True  == b = return $ fromBool True
    simplifyOp _ = na "simplifyOp{OpImply}"

instance Pretty x => Pretty (OpImply x) where
    prettyPrec prec op@(OpImply a b) = prettyPrecBinOp prec [op] a b
