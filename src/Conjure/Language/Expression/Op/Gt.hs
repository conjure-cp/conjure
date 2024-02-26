{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Gt where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpGt x = OpGt x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpGt x)
instance Hashable  x => Hashable  (OpGt x)
instance ToJSON    x => ToJSON    (OpGt x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpGt x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpGt x) where
    opLexeme _ = L_Gt

instance (TypeOf x, Pretty x) => TypeOf (OpGt x) where
    typeOf p@(OpGt a b) = sameToSameToBool p a b [] $ \case
        TypeBool -> True
        TypeInt{} | ?typeCheckerMode == RelaxedIntegerTags -> True
        TypeInt TagInt -> True
        TypeInt TagEnum{} -> True
        _ -> False

instance SimplifyOp OpGt x where
    simplifyOp _ = na "simplifyOp{OpGt}"

instance Pretty x => Pretty (OpGt x) where
    prettyPrec prec op@(OpGt a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpGt x) where
    varSymBreakingDescription (OpGt a b) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpGt")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
