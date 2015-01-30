{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Range where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpRange x = OpRange x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpRange x)
instance Hashable  x => Hashable  (OpRange x)
instance ToJSON    x => ToJSON    (OpRange x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpRange x) where parseJSON = genericParseJSON jsonOptions

instance TypeOf x => TypeOf (OpRange x) where
    typeOf (OpRange x) = do
        TypeFunction _ a <- typeOf x
        return (TypeSet a)

instance EvaluateOp OpRange where
    evaluateOp (OpRange (ConstantAbstract (AbsLitFunction xs))) =
        return (ConstantAbstract (AbsLitSet (sortNub (map snd xs))))
    evaluateOp op = na $ "evaluateOp{OpRange}:" <++> pretty (show op)

instance Pretty x => Pretty (OpRange x) where
    prettyPrec _ (OpRange a) = "range" <> prParens (pretty a)
