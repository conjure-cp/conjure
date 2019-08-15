{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.Range where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpRange x = OpRange x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpRange x)
instance Hashable  x => Hashable  (OpRange x)
instance ToJSON    x => ToJSON    (OpRange x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpRange x) where parseJSON = genericParseJSON jsonOptions

instance (Pretty x, TypeOf x) => TypeOf (OpRange x) where
    typeOf p@(OpRange x) = do
        ty <- typeOf x
        case ty of
            TypeFunction _ a -> return (TypeSet a)
            TypeSequence a   -> return (TypeSet a)
            _                -> raiseTypeError p

instance SimplifyOp OpRange x where
    simplifyOp _ = na "simplifyOp{OpRange}"

instance Pretty x => Pretty (OpRange x) where
    prettyPrec _ (OpRange a) = "range" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpRange x) where
    varSymBreakingDescription (OpRange a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpRange")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
