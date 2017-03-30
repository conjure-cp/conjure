{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.Incumbent where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpIncumbent x = OpIncumbent x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpIncumbent x)
instance Hashable  x => Hashable  (OpIncumbent x)
instance ToJSON    x => ToJSON    (OpIncumbent x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpIncumbent x) where parseJSON = genericParseJSON jsonOptions

instance (Pretty x, TypeOf x) => TypeOf (OpIncumbent x) where
    typeOf (OpIncumbent x) = typeOf x

instance EvaluateOp OpIncumbent where
    evaluateOp (OpIncumbent x) = return x

instance SimplifyOp OpIncumbent x where
    simplifyOp OpIncumbent{} = na "simplifyOp{OpIncumbent}"

instance Pretty x => Pretty (OpIncumbent x) where
    prettyPrec _ (OpIncumbent a) = "incumbent" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpIncumbent x) where
    varSymBreakingDescription (OpIncumbent a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpIncumbent")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
