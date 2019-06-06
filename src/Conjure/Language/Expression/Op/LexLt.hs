{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.LexLt where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpLexLt x = OpLexLt x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpLexLt x)
instance Hashable  x => Hashable  (OpLexLt x)
instance ToJSON    x => ToJSON    (OpLexLt x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpLexLt x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpLexLt x) where
    opLexeme _ = L_LexLt

instance (TypeOf x, Pretty x) => TypeOf (OpLexLt x) where
    typeOf p@(OpLexLt a b) = do
        tyA <- typeOf a
        tyB <- typeOf b
        if typesUnify [tyA, tyB]
            then return TypeBool
            else raiseTypeError $ vcat [ pretty p
                                       , "LHS has type:" <+> pretty tyA
                                       , "RHS has type:" <+> pretty tyB
                                       ]

instance EvaluateOp OpLexLt where
    evaluateOp (OpLexLt (viewConstantMatrix -> Just (_, xs)) (viewConstantMatrix -> Just (_, ys))) =
        return $ ConstantBool $ xs < ys
    evaluateOp op = na $ "evaluateOp{OpLexLt}:" <++> pretty (show op)

instance SimplifyOp OpLexLt x where
    simplifyOp _ = na "simplifyOp{OpLexLt}"

instance Pretty x => Pretty (OpLexLt x) where
    prettyPrec prec op@(OpLexLt a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpLexLt x) where
    varSymBreakingDescription (OpLexLt a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpLexLt")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
