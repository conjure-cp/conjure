{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Conjure.Language.Ops.Div where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpDiv x = OpDiv x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpDiv x)
instance Hashable  x => Hashable  (OpDiv x)
instance ToJSON    x => ToJSON    (OpDiv x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDiv x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpDiv x) where
    opLexeme _ = L_Div

instance TypeOf x => TypeOf (OpDiv x) where
    typeOf (OpDiv a b) = intToIntToInt a b

instance Pretty x => DomainOf (OpDiv x) x where
    domainOf op = na $ "evaluateOp{OpDiv}:" <++> pretty op

instance EvaluateOp OpDiv where
    evaluateOp p | any isUndef (universeBi p) = return $ mkUndef TypeInt $ "Has undefined children:" <+> pretty p
    evaluateOp p@(OpDiv x y)
        | y /= 0    = ConstantInt <$> (div <$> intOut x <*> intOut y)
        | otherwise = return $ mkUndef TypeInt $ "division by zero:" <+> pretty p

instance SimplifyOp OpDiv where
    simplifyOp _ _ = na "simplifyOp{OpDiv}"

instance Pretty x => Pretty (OpDiv x) where
    prettyPrec prec op@(OpDiv a b) = prettyPrecBinOp prec [op] a b
