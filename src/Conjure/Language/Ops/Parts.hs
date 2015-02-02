{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Parts where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpParts x = OpParts x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpParts x)
instance Hashable  x => Hashable  (OpParts x)
instance ToJSON    x => ToJSON    (OpParts x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpParts x) where parseJSON = genericParseJSON jsonOptions

instance TypeOf x => TypeOf (OpParts x) where
    typeOf (OpParts x) = do
        TypePartition a <- typeOf x
        return (TypeSet (TypeSet a))

instance EvaluateOp OpParts where
    evaluateOp (OpParts (ConstantAbstract (AbsLitPartition xs))) =
        return (ConstantAbstract (AbsLitSet (map (ConstantAbstract . AbsLitSet) xs)))
    evaluateOp op = na $ "evaluateOp{OpParts}:" <++> pretty (show op)

instance SimplifyOp OpParts where
    simplifyOp _ _ = na "simplifyOp{OpParts}"

instance Pretty x => Pretty (OpParts x) where
    prettyPrec _ (OpParts a) = "parts" <> prParens (pretty a)
