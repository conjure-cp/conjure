{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.Participants where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpParticipants x = OpParticipants x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpParticipants x)
instance Hashable  x => Hashable  (OpParticipants x)
instance ToJSON    x => ToJSON    (OpParticipants x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpParticipants x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpParticipants x) where
    typeOf inp@(OpParticipants p) = do
        pTy <- typeOf p
        case pTy of
            TypePartition pTyInner -> return (TypeSet pTyInner)
            _ -> raiseTypeError inp

instance SimplifyOp OpParticipants x where
    simplifyOp _ = na "simplifyOp{OpParticipants}"

instance Pretty x => Pretty (OpParticipants x) where
    prettyPrec _ (OpParticipants a) = "participants" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpParticipants x) where
    varSymBreakingDescription (OpParticipants a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpParticipants")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
