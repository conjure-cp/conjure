{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Intersect where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpIntersect x = OpIntersect x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpIntersect x)
instance Hashable  x => Hashable  (OpIntersect x)
instance ToJSON    x => ToJSON    (OpIntersect x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpIntersect x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpIntersect x) where
    opLexeme _ = L_intersect

instance (TypeOf x, Pretty x) => TypeOf (OpIntersect x) where
    typeOf p@(OpIntersect a b) = sameToSameToSame p a b
        [ TypeSet TypeAny
        , TypeMSet TypeAny
        , TypeFunction TypeAny TypeAny
        , TypeRelation [TypeAny]
        ]
        (const False)

instance SimplifyOp OpIntersect x where
    simplifyOp _ = na "simplifyOp{OpIntersect}"

instance Pretty x => Pretty (OpIntersect x) where
    prettyPrec prec op@(OpIntersect a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpIntersect x) where
    varSymBreakingDescription (OpIntersect a b) = JSON.Object $KM.fromList
        [ ("type", JSON.String "OpIntersect")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        , ("symmetricChildren", JSON.Bool True)
        ]
