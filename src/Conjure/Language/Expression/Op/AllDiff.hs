{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.AllDiff where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


data OpAllDiff x = OpAllDiff x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpAllDiff x)
instance Hashable  x => Hashable  (OpAllDiff x)
instance ToJSON    x => ToJSON    (OpAllDiff x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpAllDiff x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpAllDiff x) where
    typeOf p@(OpAllDiff x) = do
        ty <- typeOf x
        case ty of
            TypeList{} -> return TypeBool
            TypeMatrix{} -> return TypeBool
            _ -> raiseTypeError p

instance SimplifyOp OpAllDiff x where
    simplifyOp (OpAllDiff x)
        | Just xs <- listOut x
        , length xs == 1
        = return $ fromBool True
    simplifyOp _ = na "simplifyOp{OpAllDiff}"

instance Pretty x => Pretty (OpAllDiff x) where
    prettyPrec _ (OpAllDiff a) = "allDiff" <> prParens (pretty a)

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpAllDiff x) where
    varSymBreakingDescription (OpAllDiff x) | Just xs <- listOut x = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpAllDiff")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription xs)
        , ("symmetricChildren", JSON.Bool True)
        ]
    varSymBreakingDescription (OpAllDiff x) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpAllDiff")
        , ("children", varSymBreakingDescription x)
        ]
