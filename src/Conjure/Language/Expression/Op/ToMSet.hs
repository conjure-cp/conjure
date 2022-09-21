{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.ToMSet where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpToMSet x = OpToMSet x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpToMSet x)
instance Hashable  x => Hashable  (OpToMSet x)
instance ToJSON    x => ToJSON    (OpToMSet x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpToMSet x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpToMSet x) where
    typeOf p@(OpToMSet x) = do
        tx <- typeOf x
        case tx of
            TypeRelation is  -> return (TypeMSet (TypeTuple is))
            TypeSet i        -> return (TypeMSet i)
            TypeFunction i j -> return (TypeMSet (TypeTuple [i,j]))
            TypeList i       -> return (TypeMSet i)
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "The argument has type:" <+> pretty tx
                                       ]

instance SimplifyOp OpToMSet x where
    simplifyOp _ = na "simplifyOp{OpToMSet}"

instance Pretty x => Pretty (OpToMSet x) where
    prettyPrec _ (OpToMSet a) = "toMSet" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpToMSet x) where
    varSymBreakingDescription (OpToMSet a) = JSON.Object $KM.fromList
        [ ("type", JSON.String "OpToMSet")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
