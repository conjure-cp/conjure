{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.ToInt where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpToInt x = OpToInt x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpToInt x)
instance Hashable  x => Hashable  (OpToInt x)
instance ToJSON    x => ToJSON    (OpToInt x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpToInt x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpToInt x) where
    typeOf p@(OpToInt x) = do
        ty <- typeOf x
        case ty of
            TypeBool -> return $ TypeInt TagInt
            _ -> raiseTypeError $ vcat
                [ pretty p
                , "Expected type bool."
                , "But got:" <+> pretty ty
                ]

instance SimplifyOp OpToInt x where
    simplifyOp _ = na "simplifyOp{OpToInt}"

instance Pretty x => Pretty (OpToInt x) where
    prettyPrec _ (OpToInt a) = "toInt" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpToInt x) where
    varSymBreakingDescription (OpToInt a) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpToInt")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
