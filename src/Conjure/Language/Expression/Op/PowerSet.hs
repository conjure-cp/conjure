{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.PowerSet where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Language.Expression.Op.TildeLt ( ordTildeLt )

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


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

instance EvaluateOp OpPowerSet where
    evaluateOp (OpPowerSet (viewConstantSet -> Just xs)) =
        return $ ConstantAbstract $ AbsLitSet
            [ ConstantAbstract $ AbsLitSet ys
            | ys <- subsequences (sortBy ordTildeLt (sortNub xs)) ]
    evaluateOp op = na $ "evaluateOp{OpPowerSet}:" <++> pretty (show op)

instance SimplifyOp OpPowerSet x where
    simplifyOp _ = na "simplifyOp{OpPowerSet}"

instance Pretty x => Pretty (OpPowerSet x) where
    prettyPrec _ (OpPowerSet a) = "powerSet" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpPowerSet x) where
    varSymBreakingDescription (OpPowerSet a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpPowerSet")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
