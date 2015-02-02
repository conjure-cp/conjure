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
    evaluateOp (OpMinus (ConstantInt a) (ConstantInt b)) = return $ ConstantInt (a - b)
    evaluateOp (OpMinus (ConstantAbstract (AbsLitSet as)) (ConstantAbstract (AbsLitSet bs))) =
        return $ ConstantAbstract $ AbsLitSet
            [ a
            | a <- as
            , a `notElem` bs
            ]
    evaluateOp (OpMinus (ConstantAbstract (AbsLitMSet as)) (ConstantAbstract (AbsLitMSet bs))) = do
        let asHist = histogram as
            bsHist = histogram bs
            allElems = sortNub (as++bs)
        return $ ConstantAbstract $ AbsLitMSet $ concat
            [ replicate (countA - countB) e
            | e <- allElems
            , let countA = fromMaybe 0 (e `lookup` asHist)
            , let countB = fromMaybe 0 (e `lookup` bsHist)
            ]
    evaluateOp (OpMinus (ConstantAbstract (AbsLitFunction as)) (ConstantAbstract (AbsLitFunction bs))) =
        return $ ConstantAbstract $ AbsLitFunction
            [ a
            | a <- as
            , a `notElem` bs
            ]
    evaluateOp op = na $ "evaluateOp{OpMinus}:" <++> pretty (show op)

instance SimplifyOp OpMinus where
    simplifyOp _ _ = na "simplifyOp{OpMinus}"

instance Pretty x => Pretty (OpMinus x) where
    prettyPrec prec op@(OpMinus a b) = prettyPrecBinOp prec [op] a b
