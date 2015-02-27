{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.In where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpIn x = OpIn x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpIn x)
instance Hashable  x => Hashable  (OpIn x)
instance ToJSON    x => ToJSON    (OpIn x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpIn x) where parseJSON = genericParseJSON jsonOptions
instance BinaryOperator (OpIn x) where
    opLexeme _ = L_in

instance (TypeOf x, Pretty x) => TypeOf (OpIn x) where
    typeOf p@(OpIn a b) = do
        tyA      <- typeOf a
        tyB      <- typeOf b
        tyBInner <- innerTypeOf tyB
        if tyA `typeUnify` tyBInner
            then return TypeBool
            else raiseTypeError p

instance Pretty x => DomainOf (OpIn x) x where
    domainOf op = na $ "evaluateOp{OpIn}:" <++> pretty op

instance EvaluateOp OpIn where
    evaluateOp (OpIn c (ConstantAbstract (AbsLitSet      cs))) = return $ ConstantBool $ elem c cs
    evaluateOp (OpIn c (ConstantAbstract (AbsLitMSet     cs))) = return $ ConstantBool $ elem c cs
    evaluateOp (OpIn c (ConstantAbstract (AbsLitFunction cs))) =
        return $ ConstantBool $ elem c $ map (\ (i,j) -> ConstantAbstract (AbsLitTuple [i,j]) ) cs
    evaluateOp (OpIn c (ConstantAbstract (AbsLitRelation cs))) =
        return $ ConstantBool $ elem c $ map (ConstantAbstract . AbsLitTuple) cs
    evaluateOp op = na $ "evaluateOp{OpIn}:" <++> pretty (show op)

instance SimplifyOp OpIn where
    simplifyOp _ _ = na "simplifyOp{OpIn}"

instance Pretty x => Pretty (OpIn x) where
    prettyPrec prec op@(OpIn a b) = prettyPrecBinOp prec [op] a b
