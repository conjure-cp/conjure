{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Supset where

import Conjure.Prelude
import Conjure.Language.Ops.Common
import Conjure.Language.Ops.Subset


data OpSupset x = OpSupset x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSupset x)
instance Hashable  x => Hashable  (OpSupset x)
instance ToJSON    x => ToJSON    (OpSupset x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSupset x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpSupset x) where
    opLexeme _ = L_supset

instance (TypeOf x, Pretty x) => TypeOf (OpSupset x) where
    typeOf (OpSupset a b) = sameToSameToBool a b

instance Pretty x => DomainOf (OpSupset x) x where
    domainOf op = na $ "evaluateOp{OpSupset}:" <++> pretty op

instance EvaluateOp OpSupset where
    evaluateOp (OpSupset a b) = evaluateOp (OpSubset b a)

instance SimplifyOp OpSupset where
    simplifyOp _ _ = na "simplifyOp{OpSupset}"

instance Pretty x => Pretty (OpSupset x) where
    prettyPrec prec op@(OpSupset a b) = prettyPrecBinOp prec [op] a b
