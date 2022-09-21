{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.True where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpTrue x = OpTrue x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpTrue x)
instance Hashable  x => Hashable  (OpTrue x)
instance ToJSON    x => ToJSON    (OpTrue x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpTrue x) where parseJSON = genericParseJSON jsonOptions

instance TypeOf (OpTrue x) where
    typeOf (OpTrue _) = return TypeBool

instance SimplifyOp OpTrue x where
    simplifyOp _ = na "simplifyOp{OpTrue}"

instance Pretty x => Pretty (OpTrue x) where
    prettyPrec _ (OpTrue a) = "true" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpTrue x) where
    varSymBreakingDescription (OpTrue a) = JSON.Object $KM.fromList
        [ ("type", JSON.String "OpTrue")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
