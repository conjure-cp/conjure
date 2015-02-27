{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Sum where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpSum x = OpSum x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSum x)
instance Hashable  x => Hashable  (OpSum x)
instance ToJSON    x => ToJSON    (OpSum x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSum x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpSum x) where
    typeOf p@(OpSum x) = do
        ty <- typeOf x
        case ty of
            TypeList TypeAny -> return TypeInt
            TypeList TypeInt -> return TypeInt
            TypeMatrix _ TypeAny -> return TypeInt
            TypeMatrix _ TypeInt -> return TypeInt
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "The argument has type:" <+> pretty ty
                                       ]

instance (Pretty x, ExpressionLike x) => DomainOf (OpSum x) x where
    domainOf op = na $ "evaluateOp{OpSum}:" <++> pretty op

instance BinaryOperator (OpSum x) where
    opLexeme _ = L_Plus

instance EvaluateOp OpSum where
    evaluateOp p | any isUndef (universeBi p) = return $ mkUndef TypeInt $ "Has undefined children:" <+> pretty p
    evaluateOp (OpSum x) = ConstantInt . sum <$> intsOut x

instance SimplifyOp OpSum where
    simplifyOp inj (OpSum x)
        | Just xs <- listOut x
        , let filtered = filter (/=0) xs
        , length filtered /= length xs      -- there were 0's
        = return $ inj $ OpSum $ fromList filtered
    simplifyOp _ _ = na "simplifyOp{OpSum}"

instance (Pretty x, ExpressionLike x) => Pretty (OpSum x) where
    prettyPrec prec op@(OpSum x) | Just [a,b] <- listOut x = prettyPrecBinOp prec [op] a b
    prettyPrec _ (OpSum x) = "sum" <> prParens (pretty x)
