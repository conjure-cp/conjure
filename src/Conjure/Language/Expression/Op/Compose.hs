{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.Compose where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector

import Data.Permutation

import qualified Data.Semigroup as SG


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
    evaluateOp (OpCompose (viewConstantPermutation -> Just gss)
                             (viewConstantPermutation -> Just hss)) = do
       case (fromCycles gss, fromCycles hss) of
         (Right g, Right h) ->
           return $ ConstantAbstract $ AbsLitPermutation $ toCycles $ g SG.<> h
         (Left e, _) -> fail $ "evaluateOp{OpCompose}" <++> pretty (show e)
         (_, Left e) -> fail $ "evaluateOp{OpCompose}" <++> pretty (show e)        
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
