{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.ToInt where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpToInt x = OpToInt x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpToInt x)
instance Hashable  x => Hashable  (OpToInt x)
instance ToJSON    x => ToJSON    (OpToInt x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpToInt x) where parseJSON = genericParseJSON jsonOptions

instance TypeOf x => TypeOf (OpToInt x) where
    typeOf (OpToInt x) = do
        TypeBool{} <- typeOf x
        return TypeInt

instance EvaluateOp OpToInt where
    evaluateOp (OpToInt (ConstantBool False)) = return (ConstantInt 0)
    evaluateOp (OpToInt (ConstantBool True )) = return (ConstantInt 1)
    evaluateOp op = na $ "evaluateOp{OpToInt}:" <++> pretty (show op)

instance SimplifyOp OpToInt where
    simplifyOp _ _ = na "simplifyOp{OpToInt}"

instance Pretty x => Pretty (OpToInt x) where
    prettyPrec _ (OpToInt a) = "toInt" <> prParens (pretty a)
