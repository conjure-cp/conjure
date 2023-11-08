{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Not where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpNot x = OpNot x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpNot x)
instance Hashable  x => Hashable  (OpNot x)
instance ToJSON    x => ToJSON    (OpNot x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpNot x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpNot x) where
    typeOf p@(OpNot a) = do
        aTy <- typeOf a
        case aTy of
            TypeBool -> return TypeBool
            _        -> raiseTypeError $ vcat [ pretty p
                                              , "Expected type: bool"
                                              , "But got:" <+> pretty aTy
                                              ]


instance SimplifyOp OpNot x where
    simplifyOp _ = na "simplifyOp{OpNot}"

instance Pretty x => Pretty (OpNot x) where
    prettyPrec prec (OpNot a) = parensIf (prec > 2000) ("!" <> prettyPrec 2000 a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpNot x) where
    varSymBreakingDescription (OpNot a) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpNot")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
