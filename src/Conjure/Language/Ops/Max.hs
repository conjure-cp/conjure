{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Max where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpMax x = OpMax x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpMax x)
instance Hashable  x => Hashable  (OpMax x)
instance ToJSON    x => ToJSON    (OpMax x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMax x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpMax x) where
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
    evaluateOp (OpMax (ConstantAbstract (AbsLitMatrix _ xs))) = do
        is <- concatMapM intsOut xs
        return $ if null is
            then mkUndef TypeInt "Empty collection in max"
            else ConstantInt (maximum is)
    evaluateOp (OpMax (ConstantAbstract (AbsLitSet      xs))) = do
        is <- concatMapM intsOut xs
        return $ if null is
            then mkUndef TypeInt "Empty collection in max"
            else ConstantInt (maximum is)
    evaluateOp (OpMax (ConstantAbstract (AbsLitMSet     xs))) = do
        is <- concatMapM intsOut xs
        return $ if null is
            then mkUndef TypeInt "Empty collection in max"
            else ConstantInt (maximum is)
    evaluateOp _ = na "evaluateOp{OpMax}"

instance SimplifyOp OpMax where
    simplifyOp _ _ = na "simplifyOp{OpMax}"

instance (Pretty x, ExpressionLike x) => Pretty (OpMax x) where
    prettyPrec _ (OpMax x) | Just [a,b] <- listOut x = "max" <> prettyList prParens "," [a,b]
    prettyPrec _ (OpMax x) = "max" <> prParens (pretty x)
