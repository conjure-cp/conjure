{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Language.Expression.Op.Max where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpMax x = OpMax x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpMax x)
instance Hashable  x => Hashable  (OpMax x)
instance ToJSON    x => ToJSON    (OpMax x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMax x) where parseJSON = genericParseJSON jsonOptions

instance ( TypeOf x, Pretty x
         , Domain () x :< x
         ) => TypeOf (OpMax x) where
    typeOf p@(OpMax x) | Just (dom :: Domain () x) <- project x = do
        ty <- typeOf dom
        case ty of
            TypeInt NoTag -> return ty
            TypeInt (TagEnum _) -> return ty
            TypeEnum{} -> return ty
            _ -> raiseTypeError p
    typeOf p@(OpMax x) = do
        ty <- typeOf x
        tyInner <- case ty of
            TypeList tyInner -> return tyInner
            TypeMatrix _ tyInner -> return tyInner
            TypeSet tyInner -> return tyInner
            TypeMSet tyInner -> return tyInner
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "Unexpected type inside max:" <+> pretty ty
                                       ]
        case tyInner of
            TypeInt NoTag -> return ()
            TypeInt (TagEnum _) -> return ()
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "Unexpected type inside max:" <+> pretty ty
                                       ]
        return tyInner

instance EvaluateOp OpMax where
    evaluateOp p | any isUndef (childrenBi p) = return $ mkUndef (TypeInt NoTag) $ "Has undefined children:" <+> pretty p
    evaluateOp (OpMax (DomainInConstant DomainBool)) = return (ConstantBool True)
    evaluateOp (OpMax (DomainInConstant (DomainInt NoTag rs))) = do
        is <- rangesInts rs
        return $ if null is
            then mkUndef (TypeInt NoTag) "Empty collection in max"
            else ConstantInt NoTag (maximum is)
    evaluateOp (OpMax (DomainInConstant (DomainInt (TagEnum t) rs))) = do
        is <- rangesInts rs
        return $ if null is
            then mkUndef (TypeInt (TagEnum t)) "Empty collection in max"
            else ConstantInt (TagEnum t) (maximum is)
    evaluateOp (OpMax coll@(viewConstantMatrix -> Just (_, xs))) =
        case xs of
            [] -> do
                tyInner <- typeOf coll >>= innerTypeOf
                return $ mkUndef tyInner "Empty collection in max"
            (x:_) -> do
                tyInner <- typeOf x
                case tyInner of
                    TypeInt NoTag -> do
                        is <- concatMapM (intsOut "OpMax 1") xs
                        return $ ConstantInt NoTag (maximum is)
                    TypeInt (TagEnum t) -> do
                        is <- concatMapM (intsOut "OpMax 1") xs
                        return $ ConstantInt (TagEnum t) (maximum is)
                    _ -> na "evaluateOp{OpMax}"
    evaluateOp (OpMax coll@(viewConstantSet -> Just xs)) = do
        case xs of
            [] -> do
                tyInner <- typeOf coll >>= innerTypeOf
                return $ mkUndef tyInner "Empty collection in max"
            (x:_) -> do
                tyInner <- typeOf x
                case tyInner of
                    TypeInt NoTag -> do
                        is <- concatMapM (intsOut "OpMax 1") xs
                        return $ ConstantInt NoTag (maximum is)
                    TypeInt (TagEnum t) -> do
                        is <- concatMapM (intsOut "OpMax 1") xs
                        return $ ConstantInt (TagEnum t) (maximum is)
                    _ -> na "evaluateOp{OpMax}"
    evaluateOp (OpMax coll@(viewConstantMSet -> Just xs)) = do
        case xs of
            [] -> do
                tyInner <- typeOf coll >>= innerTypeOf
                return $ mkUndef tyInner "Empty collection in max"
            (x:_) -> do
                tyInner <- typeOf x
                case tyInner of
                    TypeInt NoTag -> do
                        is <- concatMapM (intsOut "OpMax 1") xs
                        return $ ConstantInt NoTag (maximum is)
                    TypeInt (TagEnum t) -> do
                        is <- concatMapM (intsOut "OpMax 1") xs
                        return $ ConstantInt (TagEnum t) (maximum is)
                    _ -> na "evaluateOp{OpMax}"
    evaluateOp _ = na "evaluateOp{OpMax}"

instance SimplifyOp OpMax x where
    simplifyOp _ = na "simplifyOp{OpMax}"

instance Pretty x => Pretty (OpMax x) where
    prettyPrec _ (OpMax x) = "max" <> prParens (pretty x)

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpMax x) where
    varSymBreakingDescription (OpMax x) | Just xs <- listOut x = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpMax")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (OpMax x) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpMax")
        , ("children", varSymBreakingDescription x)
        ]
