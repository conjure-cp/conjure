{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.Apply where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Bug

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector

import Data.List (cycle)

data OpApply x = OpApply x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpApply x)
instance Hashable  x => Hashable  (OpApply x)
instance ToJSON    x => ToJSON    (OpApply x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpApply x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpApply x) where
    typeOf inp@(OpApply p q) = do
        pTy <- typeOf p
        qTy <- typeOf q
        case (pTy, qTy) of
          (TypePermutation pTyInner, TypePermutation qTyInner) ->
              if typesUnify [pTyInner, qTyInner]
                    then return pTy
                    else raiseTypeError inp
          _ -> raiseTypeError inp

instance EvaluateOp OpApply where
    evaluateOp op@(OpApply g@(viewConstantPermutation -> Just gss)
                           h@(viewConstantPermutation -> Just hss)) = do
        gt <- typeOf g
        ht <- typeOf h
        case (gt, ht) of
          (TypePermutation TypeInt, TypePermutation TypeInt) ->
            let appI xss i = case filter (i `elem`) xss  of
                               [] -> return i
                               [k] -> return $ head $ drop 1 $ dropWhile (/= i) $ cycle k
                               _ -> bug "evaluateOp{OpApply} element should only be in one cycle of permutation"
            in ConstantAbstract . AbsLitPermutation <$> (mapM (mapM (appI gss)) hss)
          _ -> na $ "evaluateOp{OpApply} only defined for Ints right now:" <++> pretty (show op)
    evaluateOp op = na $ "evaluateOp{OpApply}:" <++> pretty (show op)

instance SimplifyOp OpApply x where
    simplifyOp _ = na "simplifyOp{OpApply}"

instance Pretty x => Pretty (OpApply x) where
    prettyPrec _ (OpApply a i) = "apply" <> prettyList prParens "," [a,i]


instance VarSymBreakingDescription x => VarSymBreakingDescription (OpApply x) where
    varSymBreakingDescription (OpApply a i) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpApply")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription i
            ])
        ]
