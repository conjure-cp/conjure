{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Conjure.Language.Ops.Leq where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpLeq x = OpLeq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpLeq x)
instance Hashable  x => Hashable  (OpLeq x)
instance ToJSON    x => ToJSON    (OpLeq x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpLeq x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpLeq x) where
    opLexeme _ = L_Leq

instance (TypeOf x, Pretty x) => TypeOf (OpLeq x) where
    typeOf (OpLeq a b) = sameToSameToBool a b

instance Pretty x => DomainOf (OpLeq x) x where
    domainOf op = na $ "evaluateOp{OpLeq}:" <++> pretty op

instance EvaluateOp OpLeq where
    evaluateOp (OpLeq x y) = return $ ConstantBool $ x <= y

instance SimplifyOp OpLeq where
    simplifyOp _ _ = na "simplifyOp{OpLeq}"

instance Pretty x => Pretty (OpLeq x) where
    prettyPrec prec op@(OpLeq a b) = prettyPrecBinOp prec [op] a b
