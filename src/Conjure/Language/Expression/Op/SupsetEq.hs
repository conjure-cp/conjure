{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.SupsetEq where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpSupsetEq x = OpSupsetEq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSupsetEq x)
instance Hashable  x => Hashable  (OpSupsetEq x)
instance ToJSON    x => ToJSON    (OpSupsetEq x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSupsetEq x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpSupsetEq x) where
    opLexeme _ = L_supsetEq

instance (TypeOf x, Pretty x) => TypeOf (OpSupsetEq x) where
    typeOf p@(OpSupsetEq a b) = sameToSameToBool p a b
        [ TypeSet TypeAny
        , TypeMSet TypeAny
        , TypeFunction TypeAny TypeAny
        , TypeRelation [TypeAny]
        ]
        (const False)

instance SimplifyOp OpSupsetEq x where
    simplifyOp _ = na "simplifyOp{OpSupsetEq}"

instance Pretty x => Pretty (OpSupsetEq x) where
    prettyPrec prec op@(OpSupsetEq a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpSupsetEq x) where
    varSymBreakingDescription (OpSupsetEq a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpSupsetEq")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
