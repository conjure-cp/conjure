{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Language.Expression.Op.Or where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


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
            TypeSet TypeBool -> return TypeBool
            TypeMSet TypeBool -> return TypeBool
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "The argument has type:" <+> pretty ty
                                       ]

instance (OpOr x :< x) => SimplifyOp OpOr x where
    simplifyOp (OpOr x)
        | Just xs <- listOut x
        , let filtered = filter (/= fromBool True) xs
        , length filtered /= length xs      -- there were true's
        = return $ fromBool True
    simplifyOp (OpOr x)
        | Just xs <- listOut x
        , let filtered = filter (/= fromBool False) xs
        , length filtered /= length xs      -- there were false's
        = return $ inject $ OpOr $ fromList filtered
    simplifyOp _ = na "simplifyOp{OpOr}"

instance (Pretty x, ExpressionLike x) => Pretty (OpOr x) where
    prettyPrec prec op@(OpOr x) | Just [a,b] <- listOut x = prettyPrecBinOp prec [op] a b
    prettyPrec _ (OpOr x) = "or" <> prParens (pretty x)

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpOr x) where
    varSymBreakingDescription (OpOr x) | Just xs <- listOut x = JSON.Object $KM.fromList
        [ ("type", JSON.String "OpOr")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (OpOr x) = JSON.Object $KM.fromList
        [ ("type", JSON.String "OpOr")
        , ("children", varSymBreakingDescription x)
        ]
