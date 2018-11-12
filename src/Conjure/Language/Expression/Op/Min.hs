{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Language.Expression.Op.Min where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpMin x = OpMin x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpMin x)
instance Hashable  x => Hashable  (OpMin x)
instance ToJSON    x => ToJSON    (OpMin x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMin x) where parseJSON = genericParseJSON jsonOptions

instance ( TypeOf x, Pretty x
         , Domain () x :< x
         ) => TypeOf (OpMin x) where
    typeOf p@(OpMin x) | Just (dom :: Domain () x) <- project x = do
        ty <- typeOf dom
        case ty of
            TypeInt NoTag -> return ty
            TypeInt AnyTag -> return ty
            TypeInt (TagEnum _) -> return ty
            TypeEnum{} -> return ty
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "Unexpected type inside min:" <+> pretty ty
                                       ]
    typeOf p@(OpMin x) = do
        ty <- typeOf x
        tyInner <- case ty of
            TypeList tyInner -> return tyInner
            TypeMatrix _ tyInner -> return tyInner
            TypeSet tyInner -> return tyInner
            TypeMSet tyInner -> return tyInner
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "Unexpected type inside min:" <+> pretty ty
                                       ]
        case tyInner of
            TypeInt NoTag -> return ()
            TypeInt AnyTag -> return ()
            TypeInt (TagEnum _) -> return ()
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "Unexpected type inside min:" <+> pretty ty
                                       ]
        return tyInner

instance EvaluateOp OpMin where
    evaluateOp p | any isUndef (childrenBi p) =
            return $ mkUndef (TypeInt AnyTag) $ "Has undefined children:" <+> pretty p
    evaluateOp p@(OpMin x)
        | Just xs <- listOut x
        , any isUndef xs =
            return $ mkUndef (TypeInt AnyTag) $ "Has undefined children:" <+> pretty p
    evaluateOp (OpMin (DomainInConstant DomainBool)) = return (ConstantBool False)
    evaluateOp (OpMin (DomainInConstant (DomainInt t rs))) = do
        is <- rangesInts rs
        return $ if null is
            then mkUndef (TypeInt AnyTag) "Empty collection in min"
            else ConstantInt t (minimum is)
    evaluateOp (OpMin coll@(viewConstantMatrix -> Just (_, xs))) = do
        case xs of
            [] -> do
                tyInner <- typeOf coll >>= innerTypeOf
                return $ mkUndef tyInner "Empty collection in min"
            (x:_) -> do
                tyInner <- typeOf x
                case tyInner of
                    TypeInt t -> do
                        is <- concatMapM (intsOut "OpMin 1") xs
                        return $ ConstantInt t (minimum is)
                    _ -> na "evaluateOp{OpMin}"
    evaluateOp (OpMin coll@(viewConstantSet -> Just xs)) = do
        case xs of
            [] -> do
                tyInner <- typeOf coll >>= innerTypeOf
                return $ mkUndef tyInner "Empty collection in min"
            (x:_) -> do
                tyInner <- typeOf x
                case tyInner of
                    TypeInt t -> do
                        is <- concatMapM (intsOut "OpMin 1") xs
                        return $ ConstantInt t (minimum is)
                    _ -> na "evaluateOp{OpMin}"
    evaluateOp (OpMin coll@(viewConstantMSet -> Just xs)) = do
        case xs of
            [] -> do
                tyInner <- typeOf coll >>= innerTypeOf
                return $ mkUndef tyInner "Empty collection in min"
            (x:_) -> do
                tyInner <- typeOf x
                case tyInner of
                    TypeInt t -> do
                        is <- concatMapM (intsOut "OpMin 1") xs
                        return $ ConstantInt t (minimum is)
                    _ -> na "evaluateOp{OpMin}"
    evaluateOp op = na $ "evaluateOp{OpMin}" <+> pretty (show op)

instance SimplifyOp OpMin x where
    simplifyOp _ = na "simplifyOp{OpMin}"

instance Pretty x => Pretty (OpMin x) where
    prettyPrec _ (OpMin x) = "min" <> prParens (pretty x)

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpMin x) where
    varSymBreakingDescription (OpMin x) | Just xs <- listOut x = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpMin")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (OpMin x) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpMin")
        , ("children", varSymBreakingDescription x)
        ]
