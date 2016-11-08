{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.Freq where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
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
            TypeMSet tyE'
                | tyE `typeUnify` tyE' -> return TypeInt
                | otherwise            -> raiseTypeError p
            TypeMatrix _ tyE'
                | tyE `typeUnify` tyE' -> return TypeInt
                | otherwise            -> raiseTypeError p
            _ -> raiseTypeError p

instance EvaluateOp OpFreq where
    evaluateOp (OpFreq (viewConstantMSet   -> Just     cs ) c) = return $ ConstantInt $ sum [ 1 | i <- cs, c == i ]
    evaluateOp (OpFreq (viewConstantMatrix -> Just (_, cs)) c) = return $ ConstantInt $ sum [ 1 | i <- cs, c == i ]
    evaluateOp op = na $ "evaluateOp{OpFreq}:" <++> pretty (show op)

instance SimplifyOp OpFreq x where
    simplifyOp _ = na "simplifyOp{OpFreq}"

instance Pretty x => Pretty (OpFreq x) where
    prettyPrec _ (OpFreq a b) = "freq" <> prettyList prParens "," [a,b]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpFreq x) where
    varSymBreakingDescription (OpFreq a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpFreq")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
