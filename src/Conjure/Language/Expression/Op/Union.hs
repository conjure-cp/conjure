{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Union where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpUnion x = OpUnion x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpUnion x)
instance Hashable  x => Hashable  (OpUnion x)
instance ToJSON    x => ToJSON    (OpUnion x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpUnion x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpUnion x) where
    opLexeme _ = L_union

instance (TypeOf x, Pretty x) => TypeOf (OpUnion x) where
    typeOf p@(OpUnion a b) = sameToSameToSame p a b
        [ TypeSet TypeAny
        , TypeMSet TypeAny
        , TypeFunction TypeAny TypeAny
        , TypeRelation [TypeAny]
        ]
        (const False)

instance SimplifyOp OpUnion x where
    simplifyOp _ = na "simplifyOp{OpUnion}"

instance Pretty x => Pretty (OpUnion x) where
    prettyPrec prec op@(OpUnion a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpUnion x) where
    varSymBreakingDescription (OpUnion a b) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpUnion")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        , ("symmetricChildren", JSON.Bool True)
        ]
