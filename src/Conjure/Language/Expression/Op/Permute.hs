{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.Permute where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Bug

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector

import Data.List (cycle)

data OpPermute x = OpPermute x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpPermute x)
instance Hashable  x => Hashable  (OpPermute x)
instance ToJSON    x => ToJSON    (OpPermute x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpPermute x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpPermute x) where
    typeOf inp@(OpPermute p i) = do
        pTy <- typeOf p
        iTy <- typeOf i
        case (pTy,iTy) of
            (TypePermutation _, thing) -> return thing
            _ -> raiseTypeError inp

instance EvaluateOp OpPermute where
    evaluateOp op@(OpPermute (viewConstantPermutation -> Just xss) i) = do
        ti <- typeOf i
        case ti of
          TypeInt _-> case filter (i `elem`) xss  of
                         [] -> return i
                         [h] -> return $ head $ drop 1 $ dropWhile (/= i) $ cycle h
                         _ -> bug "evaluateOp{OpPermute} element should only be in one cycle of permutation"
          _ -> na $ "evaluateOp{OpPermute} only defined for Ints right now:" <++> pretty (show op)
    evaluateOp op = na $ "evaluateOp{OpPermute}:" <++> pretty (show op)

instance SimplifyOp OpPermute x where
    simplifyOp _ = na "simplifyOp{OpPermute}"

instance Pretty x => Pretty (OpPermute x) where
    prettyPrec _ (OpPermute a i) = "permute" <> prettyList prParens "," [a,i]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpPermute x) where
    varSymBreakingDescription (OpPermute a i) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpPermute")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription i
            ])
        ]
