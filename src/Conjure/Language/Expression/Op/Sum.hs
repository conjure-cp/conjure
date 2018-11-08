{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Language.Expression.Op.Sum where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpSum x = OpSum x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSum x)
instance Hashable  x => Hashable  (OpSum x)
instance ToJSON    x => ToJSON    (OpSum x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSum x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpSum x) where
    typeOf p@(OpSum x) = do
        ty <- typeOf x
        innerTy <- case ty of
            TypeList t -> return t
            TypeMatrix _ t -> return t
            TypeSet t -> return t
            TypeMSet t -> return t
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "The argument has type:" <+> pretty ty
                                       ]
        case innerTy of
            TypeInt NoTag -> return (TypeInt AnyTag)
            TypeInt AnyTag -> return (TypeInt AnyTag)
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "The argument has type:" <+> pretty ty
                                       ]

instance BinaryOperator (OpSum x) where
    opLexeme _ = L_Plus

instance EvaluateOp OpSum where
    evaluateOp p | any isUndef (childrenBi p) =
            return $ mkUndef (TypeInt AnyTag) $ "Has undefined children:" <+> pretty p
    evaluateOp p@(OpSum x)
        | Just xs <- listOut x
        , any isUndef xs =
            return $ mkUndef (TypeInt AnyTag) $ "Has undefined children:" <+> pretty p
    evaluateOp (OpSum x) = ConstantInt AnyTag . sum <$> intsOut "OpSum" x

instance (OpSum x :< x) => SimplifyOp OpSum x where
    simplifyOp (OpSum x)
        | Just xs <- listOut x
        , let filtered = filter (/=0) xs
        , length filtered /= length xs      -- there were 0's
        = return $ inject $ OpSum $ fromList filtered
    simplifyOp _ = na "simplifyOp{OpSum}"

instance (Pretty x, ExpressionLike x) => Pretty (OpSum x) where
    prettyPrec prec op@(OpSum x) | Just [a,b] <- listOut x = prettyPrecBinOp prec [op] a b
    prettyPrec _ (OpSum x) = "sum" <> prParens (pretty x)

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpSum x) where
    varSymBreakingDescription (OpSum x) | Just xs <- listOut x = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpSum")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (OpSum x) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpSum")
        , ("children", varSymBreakingDescription x)
        ]
