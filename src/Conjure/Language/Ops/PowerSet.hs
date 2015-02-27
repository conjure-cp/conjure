{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.PowerSet where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpPowerSet x = OpPowerSet x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpPowerSet x)
instance Hashable  x => Hashable  (OpPowerSet x)
instance ToJSON    x => ToJSON    (OpPowerSet x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpPowerSet x) where parseJSON = genericParseJSON jsonOptions
instance (TypeOf x, Pretty x) => TypeOf (OpPowerSet x) where
    typeOf p@(OpPowerSet x) = do
        tx <- typeOf x
        case tx of
            TypeSet i -> return (TypeSet (TypeSet i))
            _ -> raiseTypeError p

instance Pretty x => DomainOf (OpPowerSet x) x where
    domainOf op = na $ "evaluateOp{OpPowerSet}:" <++> pretty op

instance EvaluateOp OpPowerSet where
    evaluateOp (OpPowerSet (ConstantAbstract (AbsLitSet xs))) =
        return $ ConstantAbstract $ AbsLitSet
            [ ConstantAbstract (AbsLitSet ys)
            | ys <- subsequences (sortNub xs) ]
    evaluateOp op = na $ "evaluateOp{OpPowerSet}:" <++> pretty (show op)

instance SimplifyOp OpPowerSet where
    simplifyOp _ _ = na "simplifyOp{OpPowerSet}"

instance Pretty x => Pretty (OpPowerSet x) where
    prettyPrec _ (OpPowerSet a) = "powerSet" <> prParens (pretty a)
