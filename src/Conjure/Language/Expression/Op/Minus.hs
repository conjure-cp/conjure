{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Minus where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


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

instance (Pretty x, TypeOf x) => DomainOf (OpMinus x) x where
    domainOf op = mkDomainAny ("OpMinus:" <++> pretty op) <$> typeOf op

instance EvaluateOp OpMinus where
    evaluateOp (OpMinus (ConstantInt a) (ConstantInt b)) = return $ ConstantInt (a - b)
    evaluateOp (OpMinus (ConstantAbstract (AbsLitSet as)) (ConstantAbstract (AbsLitSet bs))) = do
        let outs =
                [ a
                | a <- as
                , a `notElem` bs
                ]
        return $ ConstantAbstract $ AbsLitSet outs
    evaluateOp (OpMinus (ConstantAbstract (AbsLitMSet as)) (ConstantAbstract (AbsLitMSet bs))) = do
        let asHist = histogram as
            bsHist = histogram bs
            allElems = sortNub (as++bs)
            outs =
                [ replicate (fromInteger (countA - countB)) e
                | e <- allElems
                , let countA = fromMaybe 0 (e `lookup` asHist)
                , let countB = fromMaybe 0 (e `lookup` bsHist)
                ]                
        return $ ConstantAbstract $ AbsLitMSet $ concat outs
    evaluateOp (OpMinus (ConstantAbstract (AbsLitFunction as)) (ConstantAbstract (AbsLitFunction bs))) = do
        let outs =
                [ a
                | a <- as
                , a `notElem` bs
                ]
        return $ ConstantAbstract (AbsLitFunction outs)
    evaluateOp op = na $ "evaluateOp{OpMinus}:" <++> pretty (show op)

instance SimplifyOp OpMinus x where
    simplifyOp _ = na "simplifyOp{OpMinus}"

instance Pretty x => Pretty (OpMinus x) where
    prettyPrec prec op@(OpMinus a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpMinus x) where
    varSymBreakingDescription (OpMinus a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpMinus")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
