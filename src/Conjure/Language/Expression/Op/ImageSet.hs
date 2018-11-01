{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.ImageSet where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpImageSet x = OpImageSet x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpImageSet x)
instance Hashable  x => Hashable  (OpImageSet x)
instance ToJSON    x => ToJSON    (OpImageSet x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpImageSet x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpImageSet x) where
    typeOf p@(OpImageSet f x) = do
        tyF <- typeOf f
        (from, to) <- case tyF of
            TypeFunction from to -> return (from, to)
            TypeSequence      to -> return (TypeInt NoTag, to)
            _ -> raiseTypeError $ "(function application)" <+> pretty p
        xTy <- typeOf x
        if typesUnify [xTy, from]
            then return (TypeSet to)
            else raiseTypeError $ vcat
                [ pretty p
                , "f     :" <+> pretty f
                , "f type:" <+> pretty (TypeFunction from to)
                , "x     :" <+> pretty x
                , "x type:" <+> pretty xTy
                ]

instance EvaluateOp OpImageSet where
    evaluateOp (OpImageSet f@(viewConstantFunction -> Just xs) a) = do
        TypeFunction _ tyTo <- typeOf f
        case [ y | (x,y) <- xs, a == x ] of
            [y] -> return $ ConstantAbstract $ AbsLitSet [y]
            _   -> return $ TypedConstant (ConstantAbstract $ AbsLitSet []) (TypeSet tyTo)
    evaluateOp (OpImageSet f@(viewConstantSequence -> Just xs) a) = do
        TypeSequence tyTo <- typeOf f
        case [ y | (x,y) <- zip allNats xs, a == fromInt x ] of
            [y] -> return $ ConstantAbstract $ AbsLitSet [y]
            _   -> return $ TypedConstant (ConstantAbstract $ AbsLitSet []) (TypeSet tyTo)
    evaluateOp op = na $ "evaluateOp{OpImageSet}:" <++> pretty (show op)

instance SimplifyOp OpImageSet x where
    simplifyOp _ = na "simplifyOp{OpImageSet}"

instance Pretty x => Pretty (OpImageSet x) where
    prettyPrec _ (OpImageSet a b) = "imageSet" <> prettyList prParens "," [a,b]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpImageSet x) where
    varSymBreakingDescription (OpImageSet a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpImageSet")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
