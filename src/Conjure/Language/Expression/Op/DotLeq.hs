{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.DotLeq where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpDotLeq x = OpDotLeq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpDotLeq x)
instance Hashable  x => Hashable  (OpDotLeq x)
instance ToJSON    x => ToJSON    (OpDotLeq x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDotLeq x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpDotLeq x) where
    opLexeme _ = L_DotLeq

instance (TypeOf x, Pretty x) => TypeOf (OpDotLeq x) where
    typeOf p@(OpDotLeq a b) = sameToSameToBool p a b [] (const True)

instance SimplifyOp OpDotLeq x where
    simplifyOp _ = na "simplifyOp{OpDotLeq}"

instance Pretty x => Pretty (OpDotLeq x) where
    prettyPrec prec op@(OpDotLeq a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpDotLeq x) where
    varSymBreakingDescription (OpDotLeq a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpDotLeq")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
