{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Supset where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpSupset x = OpSupset x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSupset x)
instance Hashable  x => Hashable  (OpSupset x)
instance ToJSON    x => ToJSON    (OpSupset x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSupset x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpSupset x) where
    opLexeme _ = L_supset

instance (TypeOf x, Pretty x) => TypeOf (OpSupset x) where
    typeOf p@(OpSupset a b) = sameToSameToBool p a b
        [ TypeSet TypeAny
        , TypeMSet TypeAny
        , TypeFunction TypeAny TypeAny
        , TypeRelation [TypeAny]
        ]
        (const False)

instance SimplifyOp OpSupset x where
    simplifyOp _ = na "simplifyOp{OpSupset}"

instance Pretty x => Pretty (OpSupset x) where
    prettyPrec prec op@(OpSupset a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpSupset x) where
    varSymBreakingDescription (OpSupset a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpSupset")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
