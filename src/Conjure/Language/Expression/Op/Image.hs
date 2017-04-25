{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.Image where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpImage x = OpImage x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpImage x)
instance Hashable  x => Hashable  (OpImage x)
instance ToJSON    x => ToJSON    (OpImage x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpImage x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpImage x) where
    typeOf p@(OpImage f x) = do
        tyF <- typeOf f
        (from, to) <- case tyF of
            TypeFunction from to -> return (from, to)
            TypeSequence      to -> return (TypeInt, to)
            _ -> raiseTypeError $ "(function application)" <+> pretty p
        xTy <- typeOf x
        if typesUnify [xTy, from]
            then return to
            else raiseTypeError $ vcat
                [ pretty p
                , "function     :" <+> pretty f
                , "function type:" <+> pretty (TypeFunction from to)
                , "argument     :" <+> pretty x
                , "argument type:" <+> pretty xTy
                ]

instance EvaluateOp OpImage where
    evaluateOp (OpImage f@(viewConstantFunction -> Just xs) a) =
        case [ y | (x,y) <- xs, a == x ] of
            [y] -> return y
            []  -> do
                TypeFunction _ tyTo <- typeOf f
                return $ mkUndef tyTo $ vcat
                    [ "Function is not defined at this point:" <+> pretty a
                    , "Function value:" <+> pretty f
                    ]
            _   -> do
                TypeFunction _ tyTo <- typeOf f
                return $ mkUndef tyTo $ vcat
                    [ "Function is multiply defined at this point:" <+> pretty a
                    , "Function value:" <+> pretty f
                    ]
    evaluateOp (OpImage f@(viewConstantSequence -> Just xs) a) =
        case [ y | (x,y) <- zip allNats xs, a == fromInt x ] of
            [y] -> return y
            []  -> do
                TypeSequence tyTo <- typeOf f
                return $ mkUndef tyTo $ vcat
                    [ "Sequence is not defined at this point:" <+> pretty a
                    , "Sequence value:" <+> pretty f
                    ]
            _   -> do
                TypeSequence tyTo <- typeOf f
                return $ mkUndef tyTo $ vcat
                    [ "Sequence is multiply defined at this point:" <+> pretty a
                    , "Sequence value:" <+> pretty f
                    ]
    evaluateOp op = na $ "evaluateOp{OpImage}:" <++> pretty (show op)

instance SimplifyOp OpImage x where
    simplifyOp _ = na "simplifyOp{OpImage}"

instance Pretty x => Pretty (OpImage x) where
    prettyPrec _ (OpImage a b) = "image" <> prettyList prParens "," [a,b]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpImage x) where
    varSymBreakingDescription (OpImage a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpImage")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
