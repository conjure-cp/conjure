{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Gt where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpGt x = OpGt x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpGt x)
instance Hashable  x => Hashable  (OpGt x)
instance ToJSON    x => ToJSON    (OpGt x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpGt x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpGt x) where
    opLexeme _ = L_Gt

instance (TypeOf x, Pretty x) => TypeOf (OpGt x) where
    typeOf (OpGt a b) = sameToSameToBool a b

instance (Pretty x, TypeOf x) => DomainOf (OpGt x) x where
    domainOf op = mkDomainAny ("OpGt:" <++> pretty op) <$> typeOf op

instance EvaluateOp OpGt where
    evaluateOp (OpGt x y) = return $ ConstantBool $ x > y

instance SimplifyOp OpGt x where
    simplifyOp _ = na "simplifyOp{OpGt}"

instance Pretty x => Pretty (OpGt x) where
    prettyPrec prec op@(OpGt a b) = prettyPrecBinOp prec [op] a b
