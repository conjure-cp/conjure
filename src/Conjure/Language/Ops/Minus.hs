{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Minus where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpMinus x = OpMinus x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpMinus x)
instance Hashable  x => Hashable  (OpMinus x)
instance ToJSON    x => ToJSON    (OpMinus x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMinus x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpMinus x) where
    opLexeme _ = L_Minus

instance (TypeOf x, Pretty x) => TypeOf (OpMinus x) where
    typeOf p@(OpMinus a b) = do
        tya <- typeOf a
        case tya of
            TypeInt{}       -> return ()
            TypeSet{}       -> return ()
            TypeMSet{}      -> return ()
            TypeFunction{}  -> return ()
            TypeRelation{}  -> return ()
            TypePartition{} -> return ()
            _               -> raiseTypeError p
        tyb <- typeOf b
        if typesUnify [tya, tyb]
            then return $ mostDefined [tya,tyb]
            else raiseTypeError p

instance EvaluateOp OpMinus where
    evaluateOp (OpMinus (ConstantAbstract (AbsLitSet as)) (ConstantAbstract (AbsLitSet bs))) = do
        let bsNormalised = map normaliseConstant bs
        return $ ConstantAbstract $ AbsLitSet
            [ aNormalised
            | a <- as
            , let aNormalised = normaliseConstant a
            , aNormalised `notElem` bsNormalised
            ]
    evaluateOp (OpMinus x y) = ConstantInt <$> ((-) <$> intOut x <*> intOut y)

instance Pretty x => Pretty (OpMinus x) where
    prettyPrec prec op@(OpMinus a b) = prettyPrecBinOp prec [op] a b
