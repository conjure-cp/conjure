{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.DontCare where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpDontCare x = OpDontCare x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpDontCare x)
instance Hashable  x => Hashable  (OpDontCare x)
instance ToJSON    x => ToJSON    (OpDontCare x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDontCare x) where parseJSON = genericParseJSON jsonOptions

instance TypeOf x => TypeOf (OpDontCare x) where
    typeOf (OpDontCare _) = return TypeBool

instance Pretty x => DomainOf (OpDontCare x) x where
    domainOf op = na $ "evaluateOp{OpDontCare}:" <++> pretty op

instance EvaluateOp OpDontCare where
    evaluateOp op = na $ "evaluateOp{OpDontcare}:" <++> pretty (show op)

instance SimplifyOp OpDontCare x where
    simplifyOp _ = na "simplifyOp{OpDontCare}"

instance Pretty x => Pretty (OpDontCare x) where
    prettyPrec _ (OpDontCare a) = "dontCare" <> prParens (pretty a)
