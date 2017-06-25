{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.In where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


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
        case innerTypeOf tyB of
            Nothing -> raiseTypeError p
            Just tyBInner ->
                if tyA `typeUnify` tyBInner
                    then return TypeBool
                    else raiseTypeError p

instance EvaluateOp OpIn where
    evaluateOp (OpIn c (viewConstantSet      -> Just cs)) = return $ ConstantBool $ elem c cs
    evaluateOp (OpIn c (viewConstantMSet     -> Just cs)) = return $ ConstantBool $ elem c cs
    evaluateOp (OpIn c (viewConstantFunction -> Just cs)) =
        return $ ConstantBool $ elem c $ map (\ (i,j) -> ConstantAbstract $ AbsLitTuple [i,j] ) cs
    evaluateOp (OpIn c (viewConstantRelation -> Just cs)) =
        return $ ConstantBool $ elem c $ map (ConstantAbstract . AbsLitTuple) cs
    evaluateOp op = na $ "evaluateOp{OpIn}:" <++> pretty (show op)

instance SimplifyOp OpIn x where
    simplifyOp _ = na "simplifyOp{OpIn}"

instance Pretty x => Pretty (OpIn x) where
    prettyPrec prec op@(OpIn a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpIn x) where
    varSymBreakingDescription (OpIn a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpIn")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
