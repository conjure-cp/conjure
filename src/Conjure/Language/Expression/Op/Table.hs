{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Table where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpTable x = OpTable x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpTable x)
instance Hashable  x => Hashable  (OpTable x)
instance ToJSON    x => ToJSON    (OpTable x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpTable x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpTable x) where
    typeOf p@(OpTable vars table) = do
        tyVars <- typeOf vars
        tyTable <- typeOf table
        let
            tyError = raiseTypeError $ vcat [ pretty p
                                            , pretty vars <+> "has type" <+> pretty tyVars
                                            , pretty table <+> "has type" <+> pretty tyTable
                                            ]
            listLike (TypeList TypeInt{}) = return ()
            listLike (TypeMatrix _ TypeInt{}) = return ()
            listLike _ = tyError
        listLike tyVars
        case tyTable of
            TypeList inner -> listLike inner
            TypeMatrix _ inner -> listLike inner
            _ -> tyError
        return TypeBool

instance SimplifyOp OpTable x where
    simplifyOp _ = na "simplifyOp{OpTable}"

instance Pretty x => Pretty (OpTable x) where
    prettyPrec _ (OpTable a b) = "table" <> prettyList prParens "," [a, b]

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpTable x) where
    varSymBreakingDescription (OpTable a b) = JSON.Object $KM.fromList
        [ ("type", JSON.String "OpTable")
        , ("children", JSON.Array $ V.fromList [varSymBreakingDescription a, varSymBreakingDescription b])
        , ("symmetricChildren", JSON.Bool True)
        ]
