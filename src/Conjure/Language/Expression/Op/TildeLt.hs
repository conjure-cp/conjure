{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.TildeLt where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpTildeLt x = OpTildeLt x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpTildeLt x)
instance Hashable  x => Hashable  (OpTildeLt x)
instance ToJSON    x => ToJSON    (OpTildeLt x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpTildeLt x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpTildeLt x) where
    opLexeme _ = L_TildeLt

instance (TypeOf x, Pretty x) => TypeOf (OpTildeLt x) where
    typeOf p@(OpTildeLt a b) = sameToSameToBool p a b [] (const True)

instance SimplifyOp OpTildeLt x where
    simplifyOp _ = na "simplifyOp{OpTildeLt}"

instance Pretty x => Pretty (OpTildeLt x) where
    prettyPrec prec op@(OpTildeLt a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpTildeLt x) where
    varSymBreakingDescription (OpTildeLt a b) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpTildeLt")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
