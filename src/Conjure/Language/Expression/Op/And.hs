{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Language.Expression.Op.And where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpAnd x = OpAnd x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpAnd x)
instance Hashable  x => Hashable  (OpAnd x)
instance ToJSON    x => ToJSON    (OpAnd x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpAnd x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpAnd x) where
    opLexeme _ = L_And

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpAnd x) where
    typeOf p@(OpAnd x) = do
        ty <- typeOf x
        case ty of
            TypeList TypeAny -> return TypeBool
            TypeList TypeBool -> return TypeBool
            TypeMatrix _ TypeAny -> return TypeBool
            TypeMatrix _ TypeBool -> return TypeBool
            TypeSet TypeBool -> return TypeBool
            TypeMSet TypeBool -> return TypeBool
            TypeAny -> return TypeBool
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "The argument has type:" <+> pretty ty
                                       ]

instance (OpAnd x :< x) => SimplifyOp OpAnd x where
    simplifyOp (OpAnd x)
        | Just xs <- listOut x
        , let filtered = filter (/= fromBool False) xs
        , length filtered /= length xs      -- there were false's
        = return $ fromBool False
    simplifyOp (OpAnd x)
        | Just xs <- listOut x
        , let filtered = filter (/= fromBool True) xs
        , length filtered /= length xs      -- there were true's
        = return $ inject $ OpAnd $ fromList filtered
    simplifyOp _ = na "simplifyOp{OpAnd}"

instance (Pretty x, ExpressionLike x) => Pretty (OpAnd x) where
    prettyPrec prec op@(OpAnd x) | Just [a,b] <- listOut x = prettyPrecBinOp prec [op] a b
    prettyPrec _ (OpAnd x) = "and" <> prParens (pretty x)

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpAnd x) where
    varSymBreakingDescription (OpAnd x) | Just xs <- listOut x = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpAnd")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (OpAnd x) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpAnd")
        , ("children", varSymBreakingDescription x)
        ]
