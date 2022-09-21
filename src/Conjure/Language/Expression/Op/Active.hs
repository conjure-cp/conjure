{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Active where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpActive x = OpActive x Name
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpActive x)
instance Hashable  x => Hashable  (OpActive x)
instance ToJSON    x => ToJSON    (OpActive x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpActive x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpActive x) where
    typeOf p@(OpActive v n) = do
        ty <- typeOf v
        case ty of
            TypeVariant ts ->
                if elem n (map fst ts)
                    then return TypeBool
                    else raiseTypeError $ vcat [ pretty p
                                               , "The field name isn't a part of the variant type."
                                               , pretty ty
                                               ]
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "First argument has to be a variant type."
                                       , pretty ty
                                       ]

instance SimplifyOp OpActive x where
    simplifyOp _ = na "simplifyOp{OpActive}"

instance Pretty x => Pretty (OpActive x) where
    prettyPrec _ (OpActive a b) = "active" <> prettyList prParens "," [pretty a, pretty b]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpActive x) where
    varSymBreakingDescription (OpActive a b) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpActive")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , toJSON b
            ])
        ]
