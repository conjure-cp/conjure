{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.Together where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpTogether x = OpTogether x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpTogether x)
instance Hashable  x => Hashable  (OpTogether x)
instance ToJSON    x => ToJSON    (OpTogether x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpTogether x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpTogether x) where
    typeOf inp@(OpTogether x p) = do
        xTy <- typeOf x
        pTy <- typeOf p
        case (xTy, pTy) of
            (TypeSet xTyInner, TypePartition pTyInner) | typesUnify [xTyInner, pTyInner] -> return TypeBool
            (TypeSet xTyInner, TypePartitionSequence pTyInner) | typesUnify [xTyInner, pTyInner] -> return TypeBool
            _ -> raiseTypeError inp

instance EvaluateOp OpTogether where
    evaluateOp (OpTogether _ ConstantUndefined{}) = return (fromBool False)
    evaluateOp (OpTogether (viewConstantSet -> Just ys) (viewConstantPartition -> Just xss)) =
        return $ ConstantBool $ or
            [ and [ y `elem` xs | y <- ys ]
            | xs <- xss
            ]
    evaluateOp (OpTogether (viewConstantSet -> Just ys) (viewConstantPartitionSequence -> Just xss)) =
        return $ ConstantBool $ or
            [ and [ y `elem` xs | y <- ys ]
            | xs <- xss
            ]
    evaluateOp op = na $ "evaluateOp{OpTogether}:" <++> pretty (show op)

instance SimplifyOp OpTogether x where
    simplifyOp _ = na "simplifyOp{OpTogether}"

instance Pretty x => Pretty (OpTogether x) where
    prettyPrec _ (OpTogether a b) = "together" <> prettyList prParens "," [a,b]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpTogether x) where
    varSymBreakingDescription (OpTogether a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpTogether")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
