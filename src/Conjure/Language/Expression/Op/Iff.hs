{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Iff where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpIff x = OpIff x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpIff x)
instance Hashable  x => Hashable  (OpIff x)
instance ToJSON    x => ToJSON    (OpIff x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpIff x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpIff x) where
    opLexeme _ = L_Iff

instance (TypeOf x, Pretty x) => TypeOf (OpIff x) where
    typeOf p@(OpIff a b) = boolToBoolToBool p a b

instance SimplifyOp OpIff x where
    simplifyOp (OpIff a b)
        | fromBool True == a = return b
        | fromBool True == b = return a
    simplifyOp _ = na "simplifyOp{OpIff}"

instance Pretty x => Pretty (OpIff x) where
    prettyPrec prec op@(OpIff a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpIff x) where
    varSymBreakingDescription (OpIff a b) = JSON.Object $KM.fromList
        [ ("type", JSON.String "OpIff")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        , ("symmetricChildren", JSON.Bool True)
        ]
