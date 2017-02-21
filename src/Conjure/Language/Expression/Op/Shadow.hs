{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.Shadow where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpShadow x = OpShadow x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpShadow x)
instance Hashable  x => Hashable  (OpShadow x)
instance ToJSON    x => ToJSON    (OpShadow x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpShadow x) where parseJSON = genericParseJSON jsonOptions

instance (Pretty x, TypeOf x) => TypeOf (OpShadow x) where
    typeOf (OpShadow x) = typeOf x

instance EvaluateOp OpShadow where
    evaluateOp (OpShadow x) = return x

instance SimplifyOp OpShadow x where
    simplifyOp OpShadow{} = na "simplifyOp{OpShadow}"

instance Pretty x => Pretty (OpShadow x) where
    prettyPrec _ (OpShadow a) = "shadow" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpShadow x) where
    varSymBreakingDescription (OpShadow a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpShadow")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
