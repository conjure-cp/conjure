{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.Intersect where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpIntersect x = OpIntersect x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpIntersect x)
instance Hashable  x => Hashable  (OpIntersect x)
instance ToJSON    x => ToJSON    (OpIntersect x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpIntersect x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpIntersect x) where
    opLexeme _ = L_intersect

instance (TypeOf x, Pretty x) => TypeOf (OpIntersect x) where
    typeOf p@(OpIntersect a b) = sameToSameToSame p a b
                                [ TypeSet TypeAny
                                , TypeMSet TypeAny
                                , TypeFunction TypeAny TypeAny
                                , TypeRelation [TypeAny]
                                ]

instance EvaluateOp OpIntersect where
    evaluateOp p | any isUndef (childrenBi p) = do
        ty <- typeOf p
        return $ mkUndef ty $ "Has undefined children:" <+> pretty p
    evaluateOp p@(OpIntersect (viewConstantSet -> Just as) (viewConstantSet -> Just bs)) = do
        ty <- typeOf p
        let outs = sortNub [ i | i <- as, i `elem` bs]
        return $ TypedConstant (ConstantAbstract $ AbsLitSet outs) ty
    evaluateOp p@(OpIntersect (viewConstantMSet -> Just as) (viewConstantMSet -> Just bs)) = do
        ty <- typeOf p
        let asHist = histogram as
            bsHist = histogram bs
            allElems = sortNub (as++bs)
            outs =
                [ replicate (fromInteger (min countA countB)) e
                | e <- allElems
                , let countA = fromMaybe 0 (e `lookup` asHist)
                , let countB = fromMaybe 0 (e `lookup` bsHist)
                ]
        return $ TypedConstant (ConstantAbstract $ AbsLitMSet $ concat outs) ty
    evaluateOp p@(OpIntersect (viewConstantFunction -> Just as) (viewConstantFunction -> Just bs)) = do
        ty <- typeOf p
        let outs = sortNub [ i | i <- as, i `elem` bs]
        return $ TypedConstant (ConstantAbstract $ AbsLitFunction outs) ty
    evaluateOp p@(OpIntersect (viewConstantRelation -> Just as) (viewConstantRelation -> Just bs)) = do
        ty <- typeOf p
        let outs = sortNub [ i | i <- as, i `elem` bs]
        return $ TypedConstant (ConstantAbstract $ AbsLitRelation outs) ty
    evaluateOp op = na $ "evaluateOp{OpIntersect}:" <++> pretty (show op)

instance SimplifyOp OpIntersect x where
    simplifyOp _ = na "simplifyOp{OpIntersect}"

instance Pretty x => Pretty (OpIntersect x) where
    prettyPrec prec op@(OpIntersect a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpIntersect x) where
    varSymBreakingDescription (OpIntersect a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpIntersect")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        , ("symmetricChildren", JSON.Bool True)
        ]
