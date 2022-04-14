{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.AtMost where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpAtMost x = OpAtMost x -- X: variables
                           x -- o[i] is an upper bound for the nb of occurrences of v[i] in X, indexed by T
                           x -- v[i] values, indexed by T
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpAtMost x)
instance Hashable  x => Hashable  (OpAtMost x)
instance ToJSON    x => ToJSON    (OpAtMost x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpAtMost x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpAtMost x) where
    typeOf p@(OpAtMost vars occs vals) = do
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

instance SimplifyOp OpAtMost x where
    simplifyOp _ = na "simplifyOp{OpAtMost}"

instance Pretty x => Pretty (OpAtMost x) where
    prettyPrec _ (OpAtMost a b c) = "atmost" <> prettyList prParens "," [a, b, c]

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpAtMost x) where
    varSymBreakingDescription (OpAtMost a b c) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpAtMost")
        , ("children", JSON.Array $ V.fromList [ varSymBreakingDescription a
                                               , varSymBreakingDescription b
                                               , varSymBreakingDescription c
                                               ])
        , ("symmetricChildren", JSON.Bool True)
        ]
