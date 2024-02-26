{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Freq where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpFreq x = OpFreq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpFreq x)
instance Hashable  x => Hashable  (OpFreq x)
instance ToJSON    x => ToJSON    (OpFreq x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpFreq x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpFreq x) where
    typeOf p@(OpFreq m e) = do
        tyM <- typeOf m
        tyE <- typeOf e
        case tyM of
            TypeMatrix _ tyE'
                | tyE `typeUnify` tyE' -> return $ TypeInt TagInt
                | otherwise            -> raiseTypeError $ vcat
                    [ "The first argument of freq is expected to be a matrix or a multi-set."
                    , "We got:" <+> pretty tyM
                    , "In expression:" <+> pretty p
                    ]
            TypeMSet tyE'
                | tyE `typeUnify` tyE' -> return $ TypeInt TagInt
                | otherwise            -> raiseTypeError $ vcat
                    [ "The first argument of freq is expected to be a matrix or a multi-set."
                    , "We got:" <+> pretty tyM
                    , "In expression:" <+> pretty p
                    ]
            _ -> raiseTypeError $ vcat
                    [ "The first argument of freq is expected to be a matrix or a multi-set."
                    , "We got:" <+> pretty tyM
                    , "In expression:" <+> pretty p
                    ]

instance SimplifyOp OpFreq x where
    simplifyOp _ = na "simplifyOp{OpFreq}"

instance Pretty x => Pretty (OpFreq x) where
    prettyPrec _ (OpFreq a b) = "freq" <> prettyList prParens "," [a,b]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpFreq x) where
    varSymBreakingDescription (OpFreq a b) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpFreq")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
