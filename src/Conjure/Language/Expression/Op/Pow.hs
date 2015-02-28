{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Pow where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpPow x = OpPow x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpPow x)
instance Hashable  x => Hashable  (OpPow x)
instance ToJSON    x => ToJSON    (OpPow x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpPow x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpPow x) where
    opLexeme _ = L_Pow

instance TypeOf x => TypeOf (OpPow x) where
    typeOf (OpPow a b) = intToIntToInt a b

instance Pretty x => DomainOf (OpPow x) x where
    domainOf op = na $ "evaluateOp{OpPow}:" <++> pretty op

instance EvaluateOp OpPow where
    evaluateOp p | any isUndef (universeBi p) = return $ mkUndef TypeInt $ "Has undefined children:" <+> pretty p
    evaluateOp p@(OpPow x y)
        | y >= 0    = ConstantInt <$> ((^) <$> intOut x <*> intOut y)
        | otherwise = return $ mkUndef TypeInt $ "negative exponent:" <+> pretty p

instance SimplifyOp OpPow x where
    simplifyOp _ = na "simplifyOp{OpPow}"

instance Pretty x => Pretty (OpPow x) where
    prettyPrec prec op@(OpPow a b) = prettyPrecBinOp prec [op] a b
