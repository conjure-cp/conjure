{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.Subset where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpSubset x = OpSubset x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSubset x)
instance Hashable  x => Hashable  (OpSubset x)
instance ToJSON    x => ToJSON    (OpSubset x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSubset x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpSubset x) where
    opLexeme _ = L_subset

instance (TypeOf x, Pretty x) => TypeOf (OpSubset x) where
    typeOf p@(OpSubset a b) = sameToSameToBool p a b

instance EvaluateOp OpSubset where
    evaluateOp (OpSubset (viewConstantSet -> Just as) (viewConstantSet -> Just bs)) =
        return $ ConstantBool $ all (`elem` bs) as && length as <= length bs
    evaluateOp (OpSubset (viewConstantMSet -> Just as) (viewConstantMSet -> Just bs)) =
        let asHist = histogram as
            bsHist = histogram bs
            allElems = sortNub (as++bs)
        in return $ ConstantBool $ and
            [ and
                [ countA <= countB
                | e <- allElems
                , let countA = fromMaybe 0 (e `lookup` asHist)
                , let countB = fromMaybe 0 (e `lookup` bsHist)
                ]
            , or
                [ countA < countB
                | e <- allElems
                , let countA = fromMaybe 0 (e `lookup` asHist)
                , let countB = fromMaybe 0 (e `lookup` bsHist)
                ]
            ]
    evaluateOp op = na $ "evaluateOp{OpSubset}:" <++> pretty (show op)

instance SimplifyOp OpSubset x where
    simplifyOp _ = na "simplifyOp{OpSubset}"

instance Pretty x => Pretty (OpSubset x) where
    prettyPrec prec op@(OpSubset a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpSubset x) where
    varSymBreakingDescription (OpSubset a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpSubset")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
