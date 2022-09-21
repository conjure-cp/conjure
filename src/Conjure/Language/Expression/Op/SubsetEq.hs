{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.SubsetEq where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpSubsetEq x = OpSubsetEq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSubsetEq x)
instance Hashable  x => Hashable  (OpSubsetEq x)
instance ToJSON    x => ToJSON    (OpSubsetEq x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSubsetEq x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpSubsetEq x) where
    opLexeme _ = L_subsetEq

instance (TypeOf x, Pretty x) => TypeOf (OpSubsetEq x) where
    typeOf p@(OpSubsetEq a b) = sameToSameToBool p a b
        [ TypeSet TypeAny
        , TypeMSet TypeAny
        , TypeFunction TypeAny TypeAny
        , TypeRelation [TypeAny]
        ]
        (const False)

instance SimplifyOp OpSubsetEq x where
    simplifyOp _ = na "simplifyOp{OpSubsetEq}"

instance Pretty x => Pretty (OpSubsetEq x) where
    prettyPrec prec op@(OpSubsetEq a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpSubsetEq x) where
    varSymBreakingDescription (OpSubsetEq a b) = JSON.Object $KM.fromList
        [ ("type", JSON.String "OpSubsetEq")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
