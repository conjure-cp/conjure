{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.Compose where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Bug

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector

import Data.List (cycle)

data OpCompose x = OpCompose x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpCompose x)
instance Hashable  x => Hashable  (OpCompose x)
instance ToJSON    x => ToJSON    (OpCompose x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpCompose x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpCompose x) where
    typeOf inp@(OpCompose p q) = do
        pTy <- typeOf p
        qTy <- typeOf q
        case (pTy, qTy) of
          (TypePermutation pTyInner, TypePermutation qTyInner) ->
              if typesUnify [pTyInner, qTyInner]
                    then return pTy
                    else raiseTypeError inp
          _ -> raiseTypeError inp

instance EvaluateOp OpCompose where
    evaluateOp op@(OpCompose g@(viewConstantPermutation -> Just gss)
                           h@(viewConstantPermutation -> Just hss)) = do
        gt <- typeOf g
        ht <- typeOf h
        case (gt, ht) of
          (TypePermutation (TypeInt _), TypePermutation (TypeInt _)) ->
            let appI xss i = case filter (i `elem`) xss  of
                               [] -> return i
                               [k] -> return $ head $ drop 1 $ dropWhile (/= i) $ cycle k
                               _ -> bug "evaluateOp{OpCompose} element should only be in one cycle of permutation"
            in ConstantAbstract . AbsLitPermutation <$> (mapM (mapM (appI gss)) hss)
          _ -> na $ "evaluateOp{OpCompose} only defined for Ints right now:" <++> pretty (show op)
    evaluateOp op = na $ "evaluateOp{OpCompose}:" <++> pretty (show op)

instance SimplifyOp OpCompose x where
    simplifyOp _ = na "simplifyOp{OpCompose}"

instance Pretty x => Pretty (OpCompose x) where
    prettyPrec _ (OpCompose a i) = "compose" <> prettyList prParens "," [a,i]


instance VarSymBreakingDescription x => VarSymBreakingDescription (OpCompose x) where
    varSymBreakingDescription (OpCompose a i) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpCompose")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription i
            ])
        ]
