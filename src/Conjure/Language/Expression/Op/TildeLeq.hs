{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.TildeLeq where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpTildeLeq x = OpTildeLeq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpTildeLeq x)
instance Hashable  x => Hashable  (OpTildeLeq x)
instance ToJSON    x => ToJSON    (OpTildeLeq x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpTildeLeq x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpTildeLeq x) where
    opLexeme _ = L_TildeLeq

instance (TypeOf x, Pretty x) => TypeOf (OpTildeLeq x) where
    typeOf p@(OpTildeLeq a b) = sameToSameToBool p a b [] (const True)

instance SimplifyOp OpTildeLeq x where
    simplifyOp _ = na "simplifyOp{OpTildeLeq}"

instance Pretty x => Pretty (OpTildeLeq x) where
    prettyPrec prec op@(OpTildeLeq a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpTildeLeq x) where
    varSymBreakingDescription (OpTildeLeq a b) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpTildeLeq")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
