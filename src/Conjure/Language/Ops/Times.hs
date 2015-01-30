{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Times where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpTimes x = OpTimes [x]
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpTimes x)
instance Hashable  x => Hashable  (OpTimes x)
instance ToJSON    x => ToJSON    (OpTimes x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpTimes x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpTimes x) where
    opLexeme _ = L_Times

instance (TypeOf x, Pretty x) => TypeOf (OpTimes x) where
    typeOf (OpTimes [a,b]) = intToIntToInt a b
    typeOf p@(OpTimes xs) = do
        tys <- mapM typeOf xs
        if typesUnify (TypeInt:tys)
            then return TypeInt
            else raiseTypeError p
instance EvaluateOp OpTimes where
    evaluateOp (OpTimes xs) = ConstantInt . product <$> concatMapM intsOut xs

instance Pretty x => Pretty (OpTimes x) where
    prettyPrec prec op@(OpTimes [a,b]) = prettyPrecBinOp prec [op] a b
    prettyPrec _       (OpTimes  xs  ) = "product" <> prettyList prParens "," xs
