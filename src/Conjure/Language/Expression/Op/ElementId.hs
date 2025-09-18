{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.ElementId where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpElementId x = OpElementId x -- X: variables
                                 x -- indexee
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpElementId x)
instance Hashable  x => Hashable  (OpElementId x)
instance ToJSON    x => ToJSON    (OpElementId x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpElementId x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpElementId x) where
    typeOf p@(OpElementId m ix) = do
        tyM <- typeOf m
        tyI <- typeOf ix
        case tyM of
            TypeMatrix tyIndex inn
                | typesUnify [tyIndex, tyI] -> return inn
                | otherwise -> failDoc $ "Indexing with inappropriate type, matrix:" <++> vcat
                    [ "The expression:"  <+> pretty p
                    , "Indexing:"        <+> pretty m
                    , "Expected type of index:" <+> pretty tyIndex
                    , "Actual type of index  :" <+> pretty tyI
                    ]
            TypeList inn
                | typesUnify [TypeInt TagInt, tyI] -> return inn
                | otherwise -> failDoc $ "Indexing with inappropriate type, list:" <++> vcat
                    [ "The expression:"  <+> pretty p
                    , "Indexing:"        <+> pretty m
                    , "Expected type of index:" <+> pretty (TypeInt TagInt)
                    , "Actual type of index  :" <+> pretty tyI
                    ]
            _ -> failDoc $ "Indexing something other than a matrix or a tuple:" <++> vcat
                    [ "The expression:" <+> pretty p
                    , "Indexing:"       <+> pretty m
                    , "With type:"      <+> pretty tyM
                    ]

instance SimplifyOp OpElementId x where
    simplifyOp _ = na "simplifyOp{OpElementId}"

instance Pretty x => Pretty (OpElementId x) where
    prettyPrec _ (OpElementId a b) = "elementId" <> prettyList prParens "," [a, b]

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpElementId x) where
    varSymBreakingDescription (OpElementId a b) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpElementId")
        , ("children", JSON.Array $ V.fromList [ varSymBreakingDescription a
                                               , varSymBreakingDescription b
                                               ])
        ]
