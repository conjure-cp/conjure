{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.Union where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpUnion x = OpUnion x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpUnion x)
instance Hashable  x => Hashable  (OpUnion x)
instance ToJSON    x => ToJSON    (OpUnion x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpUnion x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpUnion x) where
    opLexeme _ = L_union

instance (TypeOf x, Pretty x) => TypeOf (OpUnion x) where
    typeOf p@(OpUnion a b) = sameToSameToSame p a b
        [ TypeSet TypeAny
        , TypeMSet TypeAny
        , TypeFunction TypeAny TypeAny
        , TypeRelation [TypeAny]
        ]
        (const False)

instance EvaluateOp OpUnion where
    evaluateOp p | any isUndef (childrenBi p) = do
        ty <- typeOf p
        return $ mkUndef ty $ "Has undefined children:" <+> pretty p
    evaluateOp (OpUnion (viewConstantSet -> Just as) (viewConstantSet -> Just bs)) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub (as ++ bs)
    evaluateOp (OpUnion (viewConstantMSet -> Just as) (viewConstantMSet -> Just bs)) =
        let asHist = histogram as
            bsHist = histogram bs
            allElems = sortNub (as++bs)
        in
            return $ ConstantAbstract $ AbsLitMSet $ concat
                [ replicate (fromInteger (max countA countB)) e
                | e <- allElems
                , let countA = fromMaybe 0 (e `lookup` asHist)
                , let countB = fromMaybe 0 (e `lookup` bsHist)
                ]
    -- TODO: what if the same thing is mapped to two different values? undefined behaviour?
    evaluateOp (OpUnion (viewConstantFunction -> Just as) (viewConstantFunction -> Just bs)) =
        return $ ConstantAbstract $ AbsLitFunction $ sortNub (as ++ bs)
    evaluateOp (OpUnion (viewConstantRelation -> Just as) (viewConstantRelation -> Just bs)) =
        return $ ConstantAbstract $ AbsLitRelation $ sortNub (as ++ bs)
    evaluateOp op = na $ "evaluateOp{OpUnion}:" <++> pretty (show op)

instance SimplifyOp OpUnion x where
    simplifyOp _ = na "simplifyOp{OpUnion}"

instance Pretty x => Pretty (OpUnion x) where
    prettyPrec prec op@(OpUnion a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpUnion x) where
    varSymBreakingDescription (OpUnion a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpUnion")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        , ("symmetricChildren", JSON.Bool True)
        ]
