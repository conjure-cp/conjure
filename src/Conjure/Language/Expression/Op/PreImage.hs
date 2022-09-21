{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.PreImage where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpPreImage x = OpPreImage x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpPreImage x)
instance Hashable  x => Hashable  (OpPreImage x)
instance ToJSON    x => ToJSON    (OpPreImage x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpPreImage x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpPreImage x) where
    typeOf p@(OpPreImage f x) = do
        fTy <- typeOf f
        xTy <- typeOf x
        case fTy of
            TypeFunction from to -> do
                if typesUnify [xTy, to]
                    then return (TypeSet from)
                    else raiseTypeError p
            TypeSequence to -> do
                if typesUnify [xTy, to]
                    then return (TypeSet (TypeInt TagInt))
                    else raiseTypeError p
            _ -> raiseTypeError p

instance SimplifyOp OpPreImage x where
    simplifyOp _ = na "simplifyOp{OpPreImage}"

instance Pretty x => Pretty (OpPreImage x) where
    prettyPrec _ (OpPreImage a b) = "preImage" <> prettyList prParens "," [a,b]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpPreImage x) where
    varSymBreakingDescription (OpPreImage a b) = JSON.Object $KM.fromList
        [ ("type", JSON.String "OpPreImage")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
