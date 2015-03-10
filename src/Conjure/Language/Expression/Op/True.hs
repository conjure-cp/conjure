{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.True where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpTrue x = OpTrue x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpTrue x)
instance Hashable  x => Hashable  (OpTrue x)
instance ToJSON    x => ToJSON    (OpTrue x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpTrue x) where parseJSON = genericParseJSON jsonOptions

instance TypeOf x => TypeOf (OpTrue x) where
    typeOf (OpTrue _) = return TypeBool

instance (Pretty x, TypeOf x) => DomainOf (OpTrue x) x where
    domainOf op = mkDomainAny ("OpTrue:" <++> pretty op) <$> typeOf op

instance EvaluateOp OpTrue where
    evaluateOp _ = return (fromBool True)

instance SimplifyOp OpTrue x where
    simplifyOp _ = na "simplifyOp{OpTrue}"

instance Pretty x => Pretty (OpTrue x) where
    prettyPrec _ (OpTrue a) = "true" <> prParens (pretty a)
