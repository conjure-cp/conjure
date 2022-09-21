{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Imply where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpImply x = OpImply x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpImply x)
instance Hashable  x => Hashable  (OpImply x)
instance ToJSON    x => ToJSON    (OpImply x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpImply x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpImply x) where
    opLexeme _ = L_Imply

instance (TypeOf x, Pretty x) => TypeOf (OpImply x) where
    typeOf p@(OpImply a b) = boolToBoolToBool p a b

instance SimplifyOp OpImply x where
    simplifyOp (OpImply a b)
        | fromBool True  == a = return b
        | fromBool False == a = return $ fromBool True
        | fromBool True  == b = return $ fromBool True
    simplifyOp _ = na "simplifyOp{OpImply}"

instance Pretty x => Pretty (OpImply x) where
    prettyPrec prec op@(OpImply a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpImply x) where
    varSymBreakingDescription (OpImply a b) = JSON.Object $KM.fromList
        [ ("type", JSON.String "OpImply")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
