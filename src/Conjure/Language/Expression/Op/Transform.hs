{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Transform where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpTransform x = OpTransform x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpTransform x)
instance Hashable  x => Hashable  (OpTransform x)
instance ToJSON    x => ToJSON    (OpTransform x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpTransform x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpTransform x) where
    typeOf p@(OpTransform f x) = do
        tyF <- typeOf f
        (from, to) <- case tyF of
            TypeFunction from to   -> return (from, to)
            TypeSequence      to   -> return (TypeInt TagInt, to)
            _ -> raiseTypeError $ "(transform first argument not a morphism)"
                               <+> pretty p
        if typesUnify [from, to]
            then typeOf x 
            else raiseTypeError $ vcat
                [ pretty p
                , "transform morphism not homomorphic!"
                , "morphism     :" <+> pretty f
                , "morphism type:" <+> pretty (TypeFunction from to)
                ]

instance SimplifyOp OpTransform x where
    simplifyOp _ = na "simplifyOp{OpTransform}"

instance Pretty x => Pretty (OpTransform x) where
    prettyPrec _ (OpTransform a b) = "transform" <> prettyList prParens "," [a,b]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpTransform x) where
    varSymBreakingDescription (OpTransform a b) = JSON.Object $KM.fromList
        [ ("type", JSON.String "OpTransform")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
