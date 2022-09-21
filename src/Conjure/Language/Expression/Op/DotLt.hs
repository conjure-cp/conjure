{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.DotLt where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpDotLt x = OpDotLt x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpDotLt x)
instance Hashable  x => Hashable  (OpDotLt x)
instance ToJSON    x => ToJSON    (OpDotLt x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDotLt x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpDotLt x) where
    opLexeme _ = L_DotLt

instance (TypeOf x, Pretty x) => TypeOf (OpDotLt x) where
    typeOf p@(OpDotLt a b) = sameToSameToBool p a b [] (const True)

instance SimplifyOp OpDotLt x where
    simplifyOp _ = na "simplifyOp{OpDotLt}"

instance Pretty x => Pretty (OpDotLt x) where
    prettyPrec prec op@(OpDotLt a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpDotLt x) where
    varSymBreakingDescription (OpDotLt a b) = JSON.Object $KM.fromList
        [ ("type", JSON.String "OpDotLt")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
