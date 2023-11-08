{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Flatten where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpFlatten x = OpFlatten (Maybe Int) x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpFlatten x)
instance Hashable  x => Hashable  (OpFlatten x)
instance ToJSON    x => ToJSON    (OpFlatten x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpFlatten x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpFlatten x) where
    typeOf p@(OpFlatten Nothing m) = do
        let flattenType (TypeList inner) = flattenType inner
            flattenType (TypeMatrix _ inner) = flattenType inner
            flattenType ty = ty
        ty <- typeOf m
        case ty of
            TypeList n -> return (TypeList (flattenType n))
            TypeMatrix _ n -> return (TypeList (flattenType n))
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "The argument has type:" <+> pretty ty
                                       ]
    typeOf p@(OpFlatten (Just n) m) = do
        let flattenType lvl ty | lvl < 0 = return ty
            flattenType lvl (TypeList inner) = flattenType (lvl-1) inner
            flattenType lvl (TypeMatrix _ inner) = flattenType (lvl-1) inner
            flattenType _ _ = raiseTypeError $ vcat [pretty p, "Cannot flatten" <+> pretty n <+> "levels."]
        ty <- typeOf m
        TypeList <$> flattenType n ty

instance SimplifyOp OpFlatten x where
    simplifyOp _ = na "simplifyOp{OpFlatten}"

instance Pretty x => Pretty (OpFlatten x) where
    prettyPrec _ (OpFlatten Nothing  m) = "flatten" <> prParens (pretty m)
    prettyPrec _ (OpFlatten (Just n) m) = "flatten" <> prettyList prParens "," [pretty n, pretty m]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpFlatten x) where
    varSymBreakingDescription (OpFlatten n m) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpFlatten")
        , ("children", JSON.Array $ V.fromList
            [ toJSON n
            , varSymBreakingDescription m
            ])
        ]
