{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.GCC where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpGCC x = OpGCC x -- X: variables
                     x -- v[i] values, indexed by T
                     x -- o[i] is the nb of occurrences of v[i] in X, indexed by T
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpGCC x)
instance Hashable  x => Hashable  (OpGCC x)
instance ToJSON    x => ToJSON    (OpGCC x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpGCC x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpGCC x) where
    typeOf p@(OpGCC vars vals occs) = do
        tyVars <- typeOf vars
        tyVals <- typeOf vals
        tyOccs <- typeOf occs
        let
            tyError = raiseTypeError $ vcat [ pretty p
                                            , pretty vars <+> "has type" <+> pretty tyVars
                                            , pretty vals <+> "has type" <+> pretty tyVals
                                            , pretty occs <+> "has type" <+> pretty tyOccs
                                            ]
            listLike (TypeList TypeInt{}) = return ()
            listLike (TypeMatrix _ TypeInt{}) = return ()
            listLike _ = tyError
        listLike tyVars
        listLike tyVals
        listLike tyOccs
        -- TODO: check the indices and the category of vals
        return TypeBool

instance SimplifyOp OpGCC x where
    simplifyOp _ = na "simplifyOp{OpGCC}"

instance Pretty x => Pretty (OpGCC x) where
    prettyPrec _ (OpGCC a b c) = "gcc" <> prettyList prParens "," [a, b, c]

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpGCC x) where
    varSymBreakingDescription (OpGCC a b c) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpGCC")
        , ("children", JSON.Array $ V.fromList [ varSymBreakingDescription a
                                               , varSymBreakingDescription b
                                               , varSymBreakingDescription c
                                               ])
        , ("symmetricChildren", JSON.Bool True)
        ]
