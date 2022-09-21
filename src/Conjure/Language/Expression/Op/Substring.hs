{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Substring where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpSubstring x = OpSubstring x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSubstring x)
instance Hashable  x => Hashable  (OpSubstring x)
instance ToJSON    x => ToJSON    (OpSubstring x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSubstring x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpSubstring x) where
    opLexeme _ = L_substring

instance (TypeOf x, Pretty x) => TypeOf (OpSubstring x) where
    typeOf p@(OpSubstring a b) = do
        tya <- typeOf a
        tyb <- typeOf b
        case (tya, tyb) of
            (TypeSequence{}, TypeSequence{}) -> return TypeBool
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "Unexpected types for operands:"
                                       , " - " <> pretty tya
                                       , " - " <> pretty tyb
                                       ]

instance SimplifyOp OpSubstring x where
    simplifyOp _ = na "simplifyOp{OpSubstring}"

instance Pretty x => Pretty (OpSubstring x) where
    prettyPrec prec op@(OpSubstring a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpSubstring x) where
    varSymBreakingDescription (OpSubstring a b) = JSON.Object $KM.fromList
        [ ("type", JSON.String "OpSubstring")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
