{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Or where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpOr x = OpOr [x]
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpOr x)
instance Hashable  x => Hashable  (OpOr x)
instance ToJSON    x => ToJSON    (OpOr x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpOr x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpOr x) where
    opLexeme _ = L_Or

instance (TypeOf x, Pretty x) => TypeOf (OpOr x) where
    typeOf (OpOr [a,b]) = boolToBoolToBool a b
    typeOf (OpOr [x]) = do
        TypeList TypeBool <- typeOf x
        return TypeBool
    typeOf p@(OpOr xs) = do
        tys <- mapM typeOf xs
        if typesUnify (TypeBool:tys)
            then return TypeBool
            else raiseTypeError p

instance EvaluateOp OpOr where
    evaluateOp (OpOr xs) = ConstantBool . or . concat <$> mapM boolsOut xs

instance Pretty x => Pretty (OpOr x) where
    prettyPrec prec     op@(OpOr     xs) = case xs of
        []    -> "true"
        [x]   -> "or" <> prParens (pretty x)
        [x,y] -> prettyPrecBinOp prec [op] x y
        _     -> "or" <> prettyList (prParens . prBrackets) "," xs
