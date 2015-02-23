{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Product where

import Conjure.Prelude
import Conjure.Language.Ops.Common


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

instance BinaryOperator (OpProduct x) where
    opLexeme _ = L_Times

instance EvaluateOp OpProduct where
    evaluateOp p | any isUndef (universeBi p) = return $ mkUndef TypeInt $ "Has undefined children:" <+> pretty p
    evaluateOp (OpProduct x) = ConstantInt . product <$> intsOut x

instance SimplifyOp OpProduct where
    simplifyOp _ (OpProduct x)
        | Just xs <- listOut x
        , let filtered = filter (/=0) xs
        , length filtered /= length xs      -- there were 0's
        = return 0
    simplifyOp inj (OpProduct x)
        | Just xs <- listOut x
        , let filtered = filter (/=1) xs
        , length filtered /= length xs      -- there were 1's
        = return $ inj $ OpProduct $ fromList filtered
    simplifyOp _ _ = na "simplifyOp{OpProduct}"

instance (Pretty x, ExpressionLike x) => Pretty (OpProduct x) where
    prettyPrec prec op@(OpProduct x) | Just [a,b] <- listOut x = prettyPrecBinOp prec [op] a b
    prettyPrec _ (OpProduct x) = "product" <> prParens (pretty x)
