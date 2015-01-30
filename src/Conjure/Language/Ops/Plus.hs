{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Plus where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpPlus x = OpPlus [x]
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpPlus x)
instance Hashable  x => Hashable  (OpPlus x)
instance ToJSON    x => ToJSON    (OpPlus x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpPlus x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpPlus x) where
    opLexeme _ = L_Plus

instance (TypeOf x, Pretty x) => TypeOf (OpPlus x) where
    typeOf (OpPlus [a,b]) = intToIntToInt a b
    typeOf (OpPlus [x]) = do
        TypeList TypeInt <- typeOf x
        return TypeInt
    typeOf p@(OpPlus xs) = do
        tys <- mapM typeOf xs
        if typesUnify (TypeInt:tys)
            then return TypeInt
            else raiseTypeError p

instance EvaluateOp OpPlus where
    evaluateOp (OpPlus xs) = ConstantInt . sum <$> concatMapM intsOut xs

instance Pretty x => Pretty (OpPlus x) where
    prettyPrec prec op@(OpPlus  [a,b]) = prettyPrecBinOp prec [op] a b
    prettyPrec _       (OpPlus   xs  ) = "sum" <> prettyList prParens "," xs
