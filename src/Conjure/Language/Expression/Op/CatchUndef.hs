{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.CatchUndef where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Data.Vector as V               -- vector


-- | Two arguments:
-- 1st: In-Value
-- 2nd: Default
-- Evaluates to: In-Value if it is defined
--               Default if In-Value is undefined
data OpCatchUndef x = OpCatchUndef x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpCatchUndef x)
instance Hashable  x => Hashable  (OpCatchUndef x)
instance ToJSON    x => ToJSON    (OpCatchUndef x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpCatchUndef x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpCatchUndef x) where
    typeOf p@(OpCatchUndef x d) = do
        tyX <- typeOf x
        tyD <- typeOf d
        if typesUnify [tyX, tyD]
            then return (mostDefined [tyX, tyD])
            else raiseTypeError p

instance SimplifyOp OpCatchUndef x where
    simplifyOp _ = na "simplifyOp{OpCatchUndef}"

instance Pretty x => Pretty (OpCatchUndef x) where
    prettyPrec _ (OpCatchUndef a b) = "catchUndef" <> prettyList prParens "," [a, b]

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpCatchUndef x) where
    varSymBreakingDescription (OpCatchUndef x y) = JSON.Object $ KM.fromList
        [ ("type", JSON.String "OpCatchUndef")
        , ("children", JSON.Array $ V.fromList [ varSymBreakingDescription x
                                               , varSymBreakingDescription y
                                               ])
        ]
