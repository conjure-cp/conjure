{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

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

instance Pretty x => DomainOf (OpFreq x) x where
    domainOf op = na $ "evaluateOp{OpFreq}:" <++> pretty op

instance EvaluateOp OpFreq where
    evaluateOp (OpFreq c (ConstantAbstract (AbsLitMSet cs))) = return $ ConstantInt $ sum [ 1 | i <- cs, c == i ]
    evaluateOp op = na $ "evaluateOp{OpFreq}:" <++> pretty (show op)

instance SimplifyOp OpFreq where
    simplifyOp _ _ = na "simplifyOp{OpFreq}"

instance Pretty x => Pretty (OpFreq x) where
    prettyPrec _ (OpFreq a b) = "freq" <> prettyList prParens "," [a,b]
