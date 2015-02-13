{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.FunctionImage where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpFunctionImage x = OpFunctionImage x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpFunctionImage x)
instance Hashable  x => Hashable  (OpFunctionImage x)
instance ToJSON    x => ToJSON    (OpFunctionImage x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpFunctionImage x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpFunctionImage x) where
    typeOf p@(OpFunctionImage f x) = do
        TypeFunction from to <- typeOf f
        xTy <- typeOf x
        if typesUnify [xTy, from]
            then return to
            else raiseTypeError $ vcat
                [ pretty p
                , "f     :" <+> pretty f
                , "f type:" <+> pretty (TypeFunction from to)
                , "x     :" <+> pretty x
                , "x type:" <+> pretty xTy
                ]

instance EvaluateOp OpFunctionImage where
    evaluateOp (OpFunctionImage f@(ConstantAbstract (AbsLitFunction xs)) a) = do
        ty     <- typeOf f
        let isBool = case ty of
                        TypeFunction _ TypeBool -> True
                        _                       -> False
        case [ y | (x,y) <- xs, a == x ] of
            [y] -> return y
            _ | isBool -> return $ fromBool False
            []  -> return $ mkUndef $ vcat
                    [ "Function is not defined at this point:" <+> pretty a
                    , "Function value:" <+> pretty (ConstantAbstract (AbsLitFunction xs))
                    ]
            _   -> return $ mkUndef $ vcat
                    [ "Function is multiply defined at this point:" <+> pretty a
                    , "Function value:" <+> pretty (ConstantAbstract (AbsLitFunction xs))
                    ]
    evaluateOp op = na $ "evaluateOp{OpFunctionImage}:" <++> pretty (show op)

instance SimplifyOp OpFunctionImage where
    simplifyOp _ _ = na "simplifyOp{OpFunctionImage}"

instance Pretty x => Pretty (OpFunctionImage x) where
    prettyPrec _ (OpFunctionImage a b) = "image" <> prettyList prParens "," [a,b]
