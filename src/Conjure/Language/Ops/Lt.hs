{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Lt where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpLt x = OpLt x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpLt x)
instance Hashable  x => Hashable  (OpLt x)
instance ToJSON    x => ToJSON    (OpLt x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpLt x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpLt x) where
    opLexeme _ = L_Lt

instance (TypeOf x, Pretty x) => TypeOf (OpLt x) where
    typeOf (OpLt a b) = sameToSameToBool a b

instance Pretty x => DomainOf (OpLt x) x where
    domainOf op = na $ "evaluateOp{OpLt}:" <++> pretty op

instance EvaluateOp OpLt where
    evaluateOp (OpLt x y) = return $ ConstantBool $ x < y

instance SimplifyOp OpLt where
    simplifyOp _ _ = na "simplifyOp{OpLt}"

instance Pretty x => Pretty (OpLt x) where
    prettyPrec prec op@(OpLt a b) = prettyPrecBinOp prec [op] a b
