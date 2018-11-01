{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Language.Expression.Op.SumForced where
import Conjure.Language.Expression.Op.Sum (OpSum(..))

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpSumForced x = OpSumForced x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSumForced x)
instance Hashable  x => Hashable  (OpSumForced x)
instance ToJSON    x => ToJSON    (OpSumForced x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSumForced x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpSumForced x) where
    typeOf p@(OpSumForced x) = do
        ty <- typeOf x
        case ty of
            TypeList TypeAny -> return (TypeInt NoTag)
            TypeList (TypeInt t) -> return (TypeInt t)
            TypeMatrix _ TypeAny -> return (TypeInt NoTag)
            TypeMatrix _ (TypeInt t) -> return (TypeInt t)
            TypeSet (TypeInt t) -> return (TypeInt t)
            TypeMSet (TypeInt t) -> return (TypeInt t)
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "The argument has type:" <+> pretty ty
                                       ]

instance BinaryOperator (OpSumForced x) where
    opLexeme _ = L_PlusForced

instance EvaluateOp OpSumForced where
    evaluateOp p@(OpSumForced x) | any isUndef (childrenBi p)
                 , Just t <- containsTag x =
                   return $ mkUndef (TypeInt t) $ "Has undefined children:" <+> pretty p
    evaluateOp p@(OpSumForced x)
        | Just xs <- listOut x
        , any isUndef xs
        , Just t <- containsTag x  =
          return $ mkUndef (TypeInt t) $ "Has undefined children:" <+> pretty p
    evaluateOp (OpSumForced x)
      | Just t <- containsTag x =
        ConstantInt t . sum <$> intsOut "OpSumForced" x
    evaluateOp f = fail $ "evaluateOp: OpSumForced: tag expected : " <+> stringToDoc (show f)

instance (OpSumForced x :< x) => SimplifyOp OpSumForced x where
    simplifyOp (OpSumForced x)
        | Just xs <- listOut x
        , let filtered = filter (/=0) xs
        , length filtered /= length xs      -- there were 0's
        = return $ inject $ OpSumForced $ fromList filtered
    simplifyOp _ = na "simplifyOp{OpSumForced}"

instance (Pretty x, ExpressionLike x) => Pretty (OpSumForced x) where
    prettyPrec prec (OpSumForced x) | Just [a,b] <- listOut x = prettyPrecBinOp prec [OpSum x] a b
    prettyPrec _ (OpSumForced x) = "sum" <> prParens (pretty x)

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpSumForced x) where
    varSymBreakingDescription (OpSumForced x) | Just xs <- listOut x = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpSumForced")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (OpSumForced x) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpSumForced")
        , ("children", varSymBreakingDescription x)
        ]
