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

instance ( TypeOf x, Pretty x, ExpressionLike x
         , Domain () x :< x
         ) => TypeOf (OpMax x) where
    typeOf p@(OpMax x) | Just (dom :: Domain () x) <- project x = do
        ty <- typeOf dom
        case ty of
            TypeBool{} -> return ty
            TypeInt{}  -> return ty
            TypeEnum{} -> return ty
            _ -> raiseTypeError p
    typeOf p@(OpMax x) = do
        ty <- typeOf x
        case ty of
            TypeList TypeInt -> return TypeInt
            TypeMatrix _ TypeInt -> return TypeInt
            TypeSet TypeInt -> return TypeInt
            TypeMSet TypeInt -> return TypeInt
            _ -> raiseTypeError p

instance EvaluateOp OpMax where
    evaluateOp p | any isUndef (universeBi p) = return $ mkUndef TypeInt $ "Has undefined children:" <+> pretty p
    evaluateOp (OpMax (DomainInConstant DomainBool)) = return (ConstantBool True)
    evaluateOp (OpMax (DomainInConstant (DomainInt rs))) = do
        is <- rangesInts rs
        return $ if null is
            then mkUndef TypeInt "Empty collection in max"
            else ConstantInt (maximum is)
    evaluateOp (OpMax (viewConstantMatrix -> Just (_, xs))) = do
        is <- concatMapM intsOut xs
        return $ if null is
            then mkUndef TypeInt "Empty collection in max"
            else ConstantInt (maximum is)
    evaluateOp (OpMax (viewConstantSet -> Just xs)) = do
        is <- concatMapM intsOut xs
        return $ if null is
            then mkUndef TypeInt "Empty collection in max"
            else ConstantInt (maximum is)
    evaluateOp (OpMax (viewConstantMSet -> Just xs)) = do
        is <- concatMapM intsOut xs
        return $ if null is
            then mkUndef TypeInt "Empty collection in max"
            else ConstantInt (maximum is)
    evaluateOp _ = na "evaluateOp{OpMax}"

instance SimplifyOp OpMax x where
    simplifyOp _ = na "simplifyOp{OpMax}"

instance (Pretty x, ExpressionLike x) => Pretty (OpMax x) where
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
