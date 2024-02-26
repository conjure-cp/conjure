{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Language.Expression.Op.Xor where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


-- true if an odd number of its arguments are true, and false otherwise
data OpXor x = OpXor x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpXor x)
instance Hashable  x => Hashable  (OpXor x)
instance ToJSON    x => ToJSON    (OpXor x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpXor x) where parseJSON = genericParseJSON jsonOptions

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

instance (OpXor x :< x) => SimplifyOp OpXor x where
    simplifyOp _ = na "simplifyOp{OpXor}"

instance (Pretty x, ExpressionLike x) => Pretty (OpXor x) where
    prettyPrec _ (OpXor x) = "xor" <> prParens (pretty x)

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpXor x) where
    varSymBreakingDescription (OpXor x) | Just xs <- listOut x = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpXor")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (OpXor x) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpXor")
        , ("children", varSymBreakingDescription x)
        ]
