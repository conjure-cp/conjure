{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Language.Expression.Op.Xor where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpXor x = OpXor x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpXor x)
instance Hashable  x => Hashable  (OpXor x)
instance ToJSON    x => ToJSON    (OpXor x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpXor x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpXor x) where
    opLexeme _ = L_Or

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpXor x) where
    typeOf p@(OpXor x) = do
        ty <- typeOf x
        case ty of
            TypeList TypeAny -> return TypeBool
            TypeList TypeBool -> return TypeBool
            TypeMatrix _ TypeAny -> return TypeBool
            TypeMatrix _ TypeBool -> return TypeBool
            TypeSet TypeBool -> return TypeBool
            TypeMSet TypeBool -> return TypeBool
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "The argument has type:" <+> pretty ty
                                       ]

instance EvaluateOp OpXor where
    evaluateOp (OpXor x) = ConstantBool . xor <$> boolsOut x
        where xor xs = 1 == length [ () | True <- xs ]

instance (OpXor x :< x) => SimplifyOp OpXor x where
    simplifyOp (OpXor x)
        | Just xs <- listOut x
        , let trues = filter (== fromBool True) xs
        , length trues > 1                  -- there were multiple true's
        = return $ fromBool False
    simplifyOp (OpXor x)
        | Just xs <- listOut x
        , let filtered = filter (/= fromBool False) xs
        , length filtered /= length xs      -- there were false's
        = return $ inject $ OpXor $ fromList filtered
    simplifyOp _ = na "simplifyOp{OpXor}"

instance (Pretty x, ExpressionLike x) => Pretty (OpXor x) where
    prettyPrec prec op@(OpXor x) | Just [a,b] <- listOut x = prettyPrecBinOp prec [op] a b
    prettyPrec _ (OpXor x) = "xor" <> prParens (pretty x)

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpXor x) where
    varSymBreakingDescription (OpXor x) | Just xs <- listOut x = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpXor")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (OpXor x) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpXor")
        , ("children", varSymBreakingDescription x)
        ]
