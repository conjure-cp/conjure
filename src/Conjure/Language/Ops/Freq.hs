{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Freq where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpFreq x = OpFreq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpFreq x)
instance Hashable  x => Hashable  (OpFreq x)
instance ToJSON    x => ToJSON    (OpFreq x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpFreq x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpFreq x) where
    typeOf p@(OpFreq b a) = do
        tyA <- typeOf a
        TypeMSet tyB <- typeOf b
        if tyA `typeUnify` tyB
            then return TypeInt
            else raiseTypeError p

instance EvaluateOp OpFreq where
    evaluateOp (OpFreq c (ConstantAbstract (AbsLitMSet cs))) = return $ ConstantInt $ sum [ 1 | i <- cs, c == i ]
    evaluateOp op = na $ "evaluateOp{OpFreq}:" <++> pretty (show op)

instance Pretty x => Pretty (OpFreq x) where
    prettyPrec _ (OpFreq a b) = "freq" <> prettyList prParens "," [a,b]
