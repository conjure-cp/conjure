{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.MakeTable where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpMakeTable x = OpMakeTable x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpMakeTable x)
instance Hashable  x => Hashable  (OpMakeTable x)
instance ToJSON    x => ToJSON    (OpMakeTable x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMakeTable x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpMakeTable x) where
    typeOf p@(OpMakeTable x) = do
        ty <- typeOf x
        case ty of
            TypeBool -> return TypeBool
            _ -> raiseTypeError $ vcat
                [ pretty p
                , "Expected type bool."
                , "But got:" <+> pretty ty
                ]

instance SimplifyOp OpMakeTable x where
    simplifyOp _ = na "simplifyOp{OpMakeTable}"

instance Pretty x => Pretty (OpMakeTable x) where
    prettyPrec _ (OpMakeTable a) = "makeTable" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpMakeTable x) where
    varSymBreakingDescription (OpMakeTable a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpMakeTable")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
