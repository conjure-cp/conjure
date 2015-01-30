{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.In where

import Conjure.Prelude
import Conjure.Language.Ops.Common


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
        tyA <- typeOf a
        TypeSet tyB <- typeOf b
        if tyA `typeUnify` tyB
            then return TypeBool
            else raiseTypeError p

instance EvaluateOp OpIn where
    evaluateOp (OpIn c (ConstantAbstract (AbsLitSet  cs))) = return $ ConstantBool $ elem c cs
    evaluateOp (OpIn c (ConstantAbstract (AbsLitMSet cs))) = return $ ConstantBool $ elem c cs
    evaluateOp op = na $ "evaluateOp{OpIn}:" <++> pretty (show op)

instance Pretty x => Pretty (OpIn x) where
    prettyPrec prec op@(OpIn a b) = prettyPrecBinOp prec [op] a b
