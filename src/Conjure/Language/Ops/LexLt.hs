{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.LexLt where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpLexLt x = OpLexLt x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpLexLt x)
instance Hashable  x => Hashable  (OpLexLt x)
instance ToJSON    x => ToJSON    (OpLexLt x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpLexLt x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpLexLt x) where
    opLexeme _ = L_LexLt

instance TypeOf x => TypeOf (OpLexLt x) where
    typeOf (OpLexLt a b) = do
        TypeMatrix{} <- typeOf a
        TypeMatrix{} <- typeOf b
        return TypeBool

instance EvaluateOp OpLexLt where
    evaluateOp (OpLexLt (ConstantAbstract (AbsLitMatrix _ xs)) (ConstantAbstract (AbsLitMatrix _ ys))) =
        return $ ConstantBool $ xs < ys
    evaluateOp op = na $ "evaluateOp{OpLexLt}:" <++> pretty (show op)

instance SimplifyOp OpLexLt where
    simplifyOp _ _ = na "simplifyOp{OpLexLt}"

instance Pretty x => Pretty (OpLexLt x) where
    prettyPrec prec op@(OpLexLt a b) = prettyPrecBinOp prec [op] a b
