{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.AtLeast where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpAtLeast x = OpAtLeast x -- X: variables
                             x -- o[i] is a lower bound for the nb of occurrences of v[i] in X, indexed by T
                             x -- v[i] values, indexed by T
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpAtLeast x)
instance Hashable  x => Hashable  (OpAtLeast x)
instance ToJSON    x => ToJSON    (OpAtLeast x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpAtLeast x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpAtLeast x) where
    typeOf p@(OpAtLeast vars occs vals) = do
        tyVars <- typeOf vars
        tyOccs <- typeOf occs
        tyVals <- typeOf vals
        let
            tyError = raiseTypeError $ vcat [ pretty p
                                            , pretty vars <+> "has type" <+> pretty tyVars
                                            , pretty occs <+> "has type" <+> pretty tyOccs
                                            , pretty vals <+> "has type" <+> pretty tyVals
                                            ]
            listLike (TypeList TypeInt{}) = return ()
            listLike (TypeMatrix _ TypeInt{}) = return ()
            listLike _ = tyError
        listLike tyVars
        listLike tyVals
        listLike tyOccs
        -- TODO: check the indices and the category of vals&occs
        return TypeBool

instance SimplifyOp OpAtLeast x where
    simplifyOp _ = na "simplifyOp{OpAtLeast}"

instance Pretty x => Pretty (OpAtLeast x) where
    prettyPrec _ (OpAtLeast a b c) = "atleast" <> prettyList prParens "," [a, b, c]

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpAtLeast x) where
    varSymBreakingDescription (OpAtLeast a b c) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpAtLeast")
        , ("children", JSON.Array $ V.fromList [ varSymBreakingDescription a
                                               , varSymBreakingDescription b
                                               , varSymBreakingDescription c
                                               ])
        , ("symmetricChildren", JSON.Bool True)
        ]
