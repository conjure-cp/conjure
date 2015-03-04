{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Subsequence where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpSubsequence x = OpSubsequence x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSubsequence x)
instance Hashable  x => Hashable  (OpSubsequence x)
instance ToJSON    x => ToJSON    (OpSubsequence x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSubsequence x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpSubsequence x) where
    opLexeme _ = L_subsequence

instance (TypeOf x, Pretty x) => TypeOf (OpSubsequence x) where
    typeOf p@(OpSubsequence a b) = do
        tya <- typeOf a
        tyb <- typeOf b
        case (tya, tyb) of
            (TypeSequence{}, TypeSequence{}) -> return TypeBool
            _ -> raiseTypeError p

instance DomainOf (OpSubsequence x) x where
    domainOf _ = fail "domainOf{OpSubsequence}"

instance EvaluateOp OpSubsequence where
    evaluateOp (OpSubsequence
        (ConstantAbstract (AbsLitSequence xs))
        (ConstantAbstract (AbsLitSequence ys))) =
            return $ fromBool $
                or [ and (zipWith (==) xs zs) 
                   | zs <- subsequences ys
                   , length zs >= length xs
                   ]
    evaluateOp op = na $ "evaluateOp{OpSubsequence}:" <++> pretty (show op)

instance SimplifyOp OpSubsequence x where
    simplifyOp _ = na "simplifyOp{OpSubsequence}"

instance Pretty x => Pretty (OpSubsequence x) where
    prettyPrec prec op@(OpSubsequence a b) = prettyPrecBinOp prec [op] a b
