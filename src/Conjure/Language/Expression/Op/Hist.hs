{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.Hist where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpHist x

        = OpHistAll x           -- histogram of all values
                                -- Has only one argument: the collection of values.
                                -- Produces a histogram of values in the form of a list-of-pairs,
                                -- where the first component is the value,
                                -- and the second component is the count.
                                -- There are no 0 count entries.

        | OpHistForValues x x   -- histogram of values-of-interest
                                -- Has two arguments: the collection of values,
                                -- and a list of values-of-interest.

                                -- Produces a histogram of values in the form of a list of counts,
                                -- one per each entry in the list of values-of-interest argument.
                                -- This is what's in the Essence paper, page 21, fig 4.

                                -- We extend the definition in the paper by allowing
                                -- a pair to represent lowerBound(inclusive) and upperBound(exclusive) of values.
                                -- Then, each bin contains values >=lb & <ub for the corresponding entry.

    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpHist x)
instance Hashable  x => Hashable  (OpHist x)
instance ToJSON    x => ToJSON    (OpHist x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpHist x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpHist x) where
    typeOf p@(OpHistAll a) = do
        tyA <- typeOf a
        case tyA of
            TypeMSet     aInner -> return $ TypeMatrix TypeInt $ TypeTuple [aInner, TypeInt]
            TypeMatrix _ aInner -> return $ TypeMatrix TypeInt $ TypeTuple [aInner, TypeInt]
            TypeList     aInner -> return $ TypeMatrix TypeInt $ TypeTuple [aInner, TypeInt]
            _ -> raiseTypeError p
    typeOf p@(OpHistForValues a b) = do
        tyA <- typeOf a
        tyB <- typeOf b
        case tyB of
            TypeMatrix bIndex bInner ->
                case tyA of
                    TypeMSet     aInner | typeUnify aInner bInner -> return $ TypeMatrix bIndex TypeInt
                    TypeMatrix _ aInner | typeUnify aInner bInner -> return $ TypeMatrix bIndex TypeInt
                    TypeList     aInner | typeUnify aInner bInner -> return $ TypeMatrix bIndex TypeInt
                    _ -> raiseTypeError p
            _ -> raiseTypeError p

instance EvaluateOp OpHist where
    evaluateOp (OpHistAll (viewConstantMSet -> Just cs)) = return $ ConstantAbstract $ AbsLitMatrix
        (DomainInt [RangeBounded 1 (fromInt $ genericLength $ histogram cs)])
        [ ConstantAbstract $ AbsLitTuple [e, ConstantInt n] | (e, n) <- histogram cs ]
    evaluateOp (OpHistAll (viewConstantMatrix -> Just (_, cs))) = return $ ConstantAbstract $ AbsLitMatrix
        (DomainInt [RangeBounded 1 (fromInt $ genericLength $ histogram cs)])
        [ ConstantAbstract $ AbsLitTuple [e, ConstantInt n] | (e, n) <- histogram cs ]
    evaluateOp op@(OpHistForValues
                    (viewConstantMSet -> Just cs)
                    (viewConstantMatrix -> Just (binIndex, bins))) =
        return $ ConstantAbstract $ AbsLitMatrix binIndex
            [ ConstantInt n
            | bin <- bins
            , let n = sum [ 1 | c <- cs
                              , case bin of
                                    ConstantInt{} -> c == bin
                                    ConstantAbstract (AbsLitTuple [lb, ub])
                                        -> c >= lb && c < ub
                                    _ -> bug $ "evaluateOp{OpHist}:" <++> pretty (show op)
                              ]
            ]
    evaluateOp op@(OpHistForValues
                    (viewConstantMatrix -> Just (_, cs))
                    (viewConstantMatrix -> Just (binIndex, bins))) =
        return $ ConstantAbstract $ AbsLitMatrix binIndex
            [ ConstantInt n
            | bin <- bins
            , let n = sum [ 1 | c <- cs
                              , case bin of
                                    ConstantInt{} -> c == bin
                                    ConstantAbstract (AbsLitTuple [lb, ub])
                                        -> c >= lb && c < ub
                                    _ -> bug $ "evaluateOp{OpHist}:" <++> pretty (show op)
                              ]
            ]
    evaluateOp op = na $ "evaluateOp{OpHist}:" <++> pretty (show op)

instance SimplifyOp OpHist x where
    simplifyOp _ = na "simplifyOp{OpHist}"

instance Pretty x => Pretty (OpHist x) where
    prettyPrec _ (OpHistAll a) = "hist" <> prParens (pretty a)
    prettyPrec _ (OpHistForValues a b) = "hist" <> prettyList prParens "," [a,b]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpHist x) where
    varSymBreakingDescription (OpHistAll a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpHistAll")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
    varSymBreakingDescription (OpHistForValues a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpHistForValues")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
