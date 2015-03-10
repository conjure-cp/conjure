{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Eq where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


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

instance (Pretty x, TypeOf x) => DomainOf (OpEq x) x where
    domainOf op = mkDomainAny ("OpEq:" <++> pretty op) <$> typeOf op

instance EvaluateOp OpEq where
    evaluateOp (OpEq ConstantUndefined{} _) = return $ fromBool False
    evaluateOp (OpEq _ ConstantUndefined{}) = return $ fromBool False
    evaluateOp (OpEq x y) = return $ ConstantBool $ x == y

instance SimplifyOp OpEq x where
    simplifyOp (OpEq a b)
        | fromBool True == a = return b
        | fromBool True == b = return a
    simplifyOp _ = na "simplifyOp{OpEq}"

instance Pretty x => Pretty (OpEq x) where
    prettyPrec prec op@(OpEq a b) = prettyPrecBinOp prec [op] a b
