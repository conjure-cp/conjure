{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.ToRelation where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpToRelation x = OpToRelation x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpToRelation x)
instance Hashable  x => Hashable  (OpToRelation x)
instance ToJSON    x => ToJSON    (OpToRelation x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpToRelation x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpToRelation x) where
    typeOf p@(OpToRelation x) = do
        tx <- typeOf x
        case tx of
            TypeFunction i j -> return (TypeRelation [i,j])
            _ -> raiseTypeError p

instance SimplifyOp OpToRelation x where
    simplifyOp _ = na "simplifyOp{OpToRelation}"

instance Pretty x => Pretty (OpToRelation x) where
    prettyPrec _ (OpToRelation a) = "toRelation" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpToRelation x) where
    varSymBreakingDescription (OpToRelation a) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpToRelation")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
