{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.AllDiff where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpAllDiff x = OpAllDiff x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpAllDiff x)
instance Hashable  x => Hashable  (OpAllDiff x)
instance ToJSON    x => ToJSON    (OpAllDiff x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpAllDiff x) where parseJSON = genericParseJSON jsonOptions

instance TypeOf x => TypeOf (OpAllDiff x) where
    typeOf (OpAllDiff x) = do
        TypeMatrix TypeInt TypeInt <- typeOf x
        return TypeBool

instance EvaluateOp OpAllDiff where
    evaluateOp (OpAllDiff (ConstantAbstract (AbsLitMatrix _ vals))) =
        return $ ConstantBool $ length vals == length (nub vals)
    evaluateOp op = na $ "evaluateOp{OpAllDiff}:" <++> pretty (show op)

instance Pretty x => Pretty (OpAllDiff x) where
    prettyPrec _ (OpAllDiff a) = "allDiff" <> prParens (pretty a)
