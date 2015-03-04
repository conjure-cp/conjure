{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Min where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpMin x = OpMin x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpMin x)
instance Hashable  x => Hashable  (OpMin x)
instance ToJSON    x => ToJSON    (OpMin x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMin x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpMin x) where
    typeOf p@(OpMin x) = do
        ty <- typeOf x
        case ty of
            TypeList TypeInt -> return TypeInt
            TypeMatrix _ TypeInt -> return TypeInt
            TypeSet TypeInt -> return TypeInt
            TypeMSet TypeInt -> return TypeInt
            _ -> raiseTypeError p

instance (Pretty x, ExpressionLike x) => DomainOf (OpMin x) x where
    domainOf op = na $ "evaluateOp{OpMin}:" <++> pretty op

instance EvaluateOp OpMin where
    evaluateOp p | any isUndef (universeBi p) = return $ mkUndef TypeInt $ "Has undefined children:" <+> pretty p
    evaluateOp (OpMin (ConstantAbstract (AbsLitMatrix _ xs))) = do
        is <- concatMapM intsOut xs
        return $ if null is
            then mkUndef TypeInt "Empty collection in min"
            else ConstantInt (minimum is)
    evaluateOp (OpMin (ConstantAbstract (AbsLitSet      xs))) = do
        is <- concatMapM intsOut xs
        return $ if null is
            then mkUndef TypeInt "Empty collection in min"
            else ConstantInt (minimum is)
    evaluateOp (OpMin (ConstantAbstract (AbsLitMSet     xs))) = do
        is <- concatMapM intsOut xs
        return $ if null is
            then mkUndef TypeInt "Empty collection in min"
            else ConstantInt (minimum is)
    evaluateOp _ = na "evaluateOp{OpMin}"

instance SimplifyOp OpMin x where
    simplifyOp _ = na "simplifyOp{OpMin}"

instance (Pretty x, ExpressionLike x) => Pretty (OpMin x) where
    prettyPrec _ (OpMin x) = "min" <> prParens (pretty x)
