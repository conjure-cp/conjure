{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Language.Expression.Op.Max where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpMax x = OpMax x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpMax x)
instance Hashable  x => Hashable  (OpMax x)
instance ToJSON    x => ToJSON    (OpMax x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMax x) where parseJSON = genericParseJSON jsonOptions

instance ( TypeOf x, Pretty x
         , Domain () x :< x
         ) => TypeOf (OpMax x) where
    typeOf p@(OpMax x) | Just (dom :: Domain () x) <- project x = do
        ty <- typeOfDomain dom
        case ty of
            TypeInt TagInt -> return ty
            TypeInt (TagEnum _) -> return ty
            TypeEnum{} -> return ty
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "Unexpected type inside min:" <+> pretty ty
                                       ]
    typeOf p@(OpMax x) = do
        ty <- typeOf x
        tyInner <- case ty of
            TypeList tyInner -> return tyInner
            TypeMatrix _ tyInner -> return tyInner
            TypeSet tyInner -> return tyInner
            TypeMSet tyInner -> return tyInner
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "Unexpected type inside max:" <+> pretty ty
                                       ]
        case tyInner of
            TypeInt TagInt -> return ()
            TypeInt (TagEnum _) -> return ()
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "Unexpected type inside max:" <+> pretty ty
                                       ]
        return tyInner

instance SimplifyOp OpMax x where
    simplifyOp _ = na "simplifyOp{OpMax}"

instance Pretty x => Pretty (OpMax x) where
    prettyPrec _ (OpMax x) = "max" <> prParens (pretty x)

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpMax x) where
    varSymBreakingDescription (OpMax x) | Just xs <- listOut x = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpMax")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (OpMax x) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpMax")
        , ("children", varSymBreakingDescription x)
        ]
