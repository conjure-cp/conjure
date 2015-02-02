{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.DontCare where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpDontCare x = OpDontCare x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpDontCare x)
instance Hashable  x => Hashable  (OpDontCare x)
instance ToJSON    x => ToJSON    (OpDontCare x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDontCare x) where parseJSON = genericParseJSON jsonOptions

instance TypeOf x => TypeOf (OpDontCare x) where
    typeOf (OpDontCare _) = return TypeBool

instance EvaluateOp OpDontCare where
    evaluateOp op = na $ "evaluateOp{OpDontcare}:" <++> pretty (show op)

instance SimplifyOp OpDontCare where
    simplifyOp _ _ = na "simplifyOp{OpDontCare}"

instance Pretty x => Pretty (OpDontCare x) where
    prettyPrec _ (OpDontCare a) = "dontCare" <> prParens (pretty a)
