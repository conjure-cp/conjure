{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Language.Expression.Op.Product where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpProduct x = OpProduct x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpProduct x)
instance Hashable  x => Hashable  (OpProduct x)
instance ToJSON    x => ToJSON    (OpProduct x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpProduct x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpProduct x) where
    typeOf p@(OpProduct x) = do
        ty <- typeOf x
        case ty of
            TypeList TypeAny -> return TypeInt
            TypeList TypeInt -> return TypeInt
            TypeMatrix _ TypeAny -> return TypeInt
            TypeMatrix _ TypeInt -> return TypeInt
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "The argument has type:" <+> pretty ty
                                       ]

instance (Pretty x, ExpressionLike x, TypeOf x) => DomainOf (OpProduct x) x where
    domainOf op = mkDomainAny ("OpProduct:" <++> pretty op) <$> typeOf op

instance BinaryOperator (OpProduct x) where
    opLexeme _ = L_Times

instance EvaluateOp OpProduct where
    evaluateOp p | any isUndef (universeBi p) = return $ mkUndef TypeInt $ "Has undefined children:" <+> pretty p
    evaluateOp (OpProduct x) = ConstantInt . product <$> intsOut x

instance (OpProduct x :< x) => SimplifyOp OpProduct x where
    simplifyOp (OpProduct x)
        | Just xs <- listOut x
        , let filtered = filter (/=0) xs
        , length filtered /= length xs      -- there were 0's
        = return 0
    simplifyOp (OpProduct x)
        | Just xs <- listOut x
        , let filtered = filter (/=1) xs
        , length filtered /= length xs      -- there were 1's
        = return $ inject $ OpProduct $ fromList filtered
    simplifyOp _ = na "simplifyOp{OpProduct}"

instance (Pretty x, ExpressionLike x) => Pretty (OpProduct x) where
    prettyPrec prec op@(OpProduct x) | Just [a,b] <- listOut x = prettyPrecBinOp prec [op] a b
    prettyPrec _ (OpProduct x) = "product" <> prParens (pretty x)
