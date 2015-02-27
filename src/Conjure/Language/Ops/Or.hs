{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Or where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpOr x = OpOr x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpOr x)
instance Hashable  x => Hashable  (OpOr x)
instance ToJSON    x => ToJSON    (OpOr x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpOr x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpOr x) where
    opLexeme _ = L_Or

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpOr x) where
    typeOf p@(OpOr x) = do
        ty <- typeOf x
        case ty of
            TypeList TypeAny -> return TypeBool
            TypeList TypeBool -> return TypeBool
            TypeMatrix _ TypeAny -> return TypeBool
            TypeMatrix _ TypeBool -> return TypeBool
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "The argument has type:" <+> pretty ty
                                       ]

instance (Pretty x, ExpressionLike x) => DomainOf (OpOr x) x where
    domainOf op = na $ "evaluateOp{OpOr}:" <++> pretty op

instance EvaluateOp OpOr where
    evaluateOp (OpOr x) = ConstantBool . or <$> boolsOut x

instance SimplifyOp OpOr where
    simplifyOp _ (OpOr x)
        | Just xs <- listOut x
        , let filtered = filter (/= fromBool True) xs
        , length filtered /= length xs      -- there were true's
        = return $ fromBool True
    simplifyOp inj (OpOr x)
        | Just xs <- listOut x
        , let filtered = filter (/= fromBool False) xs
        , length filtered /= length xs      -- there were false's
        = return $ inj $ OpOr $ fromList filtered
    simplifyOp _ _ = na "simplifyOp{OpOr}"

instance (Pretty x, ExpressionLike x) => Pretty (OpOr x) where
    prettyPrec prec op@(OpOr x) | Just [a,b] <- listOut x = prettyPrecBinOp prec [op] a b
    prettyPrec _ (OpOr x) = "or" <> prParens (pretty x)
