{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Mod where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpMod x = OpMod x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpMod x)
instance Hashable  x => Hashable  (OpMod x)
instance ToJSON    x => ToJSON    (OpMod x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMod x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpMod x) where
    opLexeme _ = L_Mod

instance TypeOf x => TypeOf (OpMod x) where
    typeOf (OpMod a b) = intToIntToInt a b

instance (Pretty x, TypeOf x) => DomainOf (OpMod x) x where
    domainOf op = mkDomainAny ("OpMod:" <++> pretty op) <$> typeOf op

instance EvaluateOp OpMod where
    evaluateOp p | any isUndef (universeBi p) = return $ mkUndef TypeInt $ "Has undefined children:" <+> pretty p
    evaluateOp p@(OpMod x y)
        | y /= 0    = ConstantInt <$> (mod <$> intOut x <*> intOut y)
        | otherwise = return $ mkUndef TypeInt $ "modulo zero:" <+> pretty p

instance SimplifyOp OpMod x where
    simplifyOp _ = na "simplifyOp{OpMod}"

instance Pretty x => Pretty (OpMod x) where
    prettyPrec prec op@(OpMod a b) = prettyPrecBinOp prec [op] a b
