{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.DontCare where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpDontCare x = OpDontCare x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpDontCare x)
instance Hashable  x => Hashable  (OpDontCare x)
instance ToJSON    x => ToJSON    (OpDontCare x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDontCare x) where parseJSON = genericParseJSON jsonOptions

instance TypeOf (OpDontCare x) where
    typeOf (OpDontCare _) = return TypeBool

instance SimplifyOp OpDontCare x where
    simplifyOp _ = na "simplifyOp{OpDontCare}"

instance Pretty x => Pretty (OpDontCare x) where
    prettyPrec _ (OpDontCare a) = "dontCare" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpDontCare x) where
    varSymBreakingDescription (OpDontCare a) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpDontCare")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
