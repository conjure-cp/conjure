{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.FromSolution where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpFromSolution x = OpFromSolution x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpFromSolution x)
instance Hashable  x => Hashable  (OpFromSolution x)
instance ToJSON    x => ToJSON    (OpFromSolution x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpFromSolution x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpFromSolution x) where
    typeOf (OpFromSolution x) = typeOf x

instance SimplifyOp OpFromSolution x where
    simplifyOp _ = na "simplifyOp{OpFromSolution}"

instance Pretty x => Pretty (OpFromSolution x) where
    prettyPrec _ (OpFromSolution a) = "fromSolution" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpFromSolution x) where
    varSymBreakingDescription (OpFromSolution a) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpFromSolution")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
