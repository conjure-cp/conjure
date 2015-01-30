{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.And where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpAnd x = OpAnd [x]
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpAnd x)
instance Hashable  x => Hashable  (OpAnd x)
instance ToJSON    x => ToJSON    (OpAnd x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpAnd x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpAnd x) where
    opLexeme _ = L_And

instance (TypeOf x, Pretty x) => TypeOf (OpAnd x) where
    typeOf (OpAnd [a,b]) = boolToBoolToBool a b
    typeOf (OpAnd [x]) = do
        TypeList TypeBool <- typeOf x
        return TypeBool
    typeOf p@(OpAnd xs) = do
        tys <- mapM typeOf xs
        if typesUnify (TypeBool:tys)
            then return TypeBool
            else raiseTypeError p

instance EvaluateOp OpAnd where
    evaluateOp (OpAnd xs) = ConstantBool . and . concat <$> mapM boolsOut xs

instance (Pretty x) => Pretty (OpAnd x) where
    prettyPrec prec op@(OpAnd xs) = case xs of
        []    -> "false"
        [x]   -> "and" <> prParens (pretty x)
        [x,y] -> prettyPrecBinOp prec [op] x y
        _     -> "and" <> prettyList (prParens . prBrackets) "," xs
