{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Negate where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpNegate x = OpNegate x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpNegate x)
instance Hashable  x => Hashable  (OpNegate x)
instance ToJSON    x => ToJSON    (OpNegate x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpNegate x) where parseJSON = genericParseJSON jsonOptions

instance TypeOf x => TypeOf (OpNegate x) where
    typeOf (OpNegate a) = do TypeInt <- typeOf a ; return TypeInt

instance EvaluateOp OpNegate where
    evaluateOp (OpNegate x) = ConstantInt . negate <$> intOut x

instance SimplifyOp OpNegate where
    simplifyOp _ _ = na "simplifyOp{OpNegate}"

instance Pretty x => Pretty (OpNegate x) where
    prettyPrec _ (OpNegate a) = "-" <> prettyPrec 10000 a
