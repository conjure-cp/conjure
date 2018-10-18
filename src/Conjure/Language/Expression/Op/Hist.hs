{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.Hist where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpHist x = OpHist x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpHist x)
instance Hashable  x => Hashable  (OpHist x)
instance ToJSON    x => ToJSON    (OpHist x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpHist x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpHist x) where
    typeOf p@(OpHist a) = do
        tyA <- typeOf a
        case tyA of
            TypeMSet     aInner -> return $ TypeMatrix (TypeInt Nothing) $ TypeTuple [aInner, TypeInt Nothing]
            TypeMatrix _ aInner -> return $ TypeMatrix (TypeInt Nothing) $ TypeTuple [aInner, TypeInt Nothing]
            TypeList     aInner -> return $ TypeMatrix (TypeInt Nothing) $ TypeTuple [aInner, TypeInt Nothing]
            _ -> raiseTypeError p

instance EvaluateOp OpHist where
    evaluateOp (OpHist (viewConstantMSet -> Just cs)) = return $ ConstantAbstract $ AbsLitMatrix
        (DomainInt Nothing [RangeBounded 1 (fromInt $ genericLength $ histogram cs)])
        [ ConstantAbstract $ AbsLitTuple [e, ConstantInt Nothing n] | (e, n) <- histogram cs ]
    evaluateOp (OpHist (viewConstantMatrix -> Just (_, cs))) = return $ ConstantAbstract $ AbsLitMatrix
        (DomainInt Nothing [RangeBounded 1 (fromInt $ genericLength $ histogram cs)])
        [ ConstantAbstract $ AbsLitTuple [e, ConstantInt Nothing n] | (e, n) <- histogram cs ]
    evaluateOp op = na $ "evaluateOp{OpHist}:" <++> pretty (show op)

instance SimplifyOp OpHist x where
    simplifyOp _ = na "simplifyOp{OpHist}"

instance Pretty x => Pretty (OpHist x) where
    prettyPrec _ (OpHist a) = "hist" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpHist x) where
    varSymBreakingDescription (OpHist a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpHist")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
