{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.PermutationTuples where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Bug

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector

import Data.List (cycle)

data OpPermutationTuples x = OpPermutationTuples x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpPermutationTuples x)
instance Hashable  x => Hashable  (OpPermutationTuples x)
instance ToJSON    x => ToJSON    (OpPermutationTuples x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpPermutationTuples x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpPermutationTuples x) where
    typeOf inp@(OpPermutationTuples p) = do
        pTy <- typeOf p
        case (pTy) of
          (TypePermutation pTyInner) ->
              return $ TypeSet $ TypeTuple [pTyInner, pTyInner] 
          _ -> raiseTypeError inp

instance EvaluateOp OpPermutationTuples where
    evaluateOp op = na $ "evaluateOp{OpPermutationTuples}:" <++> pretty (show op)

instance SimplifyOp OpPermutationTuples x where
    simplifyOp _ = na "simplifyOp{OpPermutationTuples}"

instance Pretty x => Pretty (OpPermutationTuples x) where
    prettyPrec _ (OpPermutationTuples a) = "permutationTuples" <> prettyList prParens "," [a]


instance VarSymBreakingDescription x => VarSymBreakingDescription (OpPermutationTuples x) where
    varSymBreakingDescription (OpPermutationTuples a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpPermutationTuples")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
