{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.AttributeAsConstraint where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpAttributeAsConstraint x = OpAttributeAsConstraint x
                                                         AttrName   -- attribute name
                                                         (Maybe x)  -- it's value
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpAttributeAsConstraint x)
instance Hashable  x => Hashable  (OpAttributeAsConstraint x)
instance ToJSON    x => ToJSON    (OpAttributeAsConstraint x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpAttributeAsConstraint x) where parseJSON = genericParseJSON jsonOptions

instance TypeOf (OpAttributeAsConstraint x) where
    -- can check more here
    typeOf OpAttributeAsConstraint{} = return TypeBool

instance EvaluateOp OpAttributeAsConstraint where
    evaluateOp _ = na "evaluateOp{OpAttributeAsConstraint}"

instance SimplifyOp OpAttributeAsConstraint x where
    simplifyOp _ = na "simplifyOp{OpAttributeAsConstraint}"

instance Pretty x => Pretty (OpAttributeAsConstraint x) where
    prettyPrec _ (OpAttributeAsConstraint x attr Nothing   ) = pretty attr <> prParens (pretty x)
    prettyPrec _ (OpAttributeAsConstraint x attr (Just val)) = pretty attr <> prettyList prParens "," [x, val]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpAttributeAsConstraint x) where
    varSymBreakingDescription (OpAttributeAsConstraint a b c) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpAttributeAsConstraint")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , toJSON b
            , maybe JSON.Null varSymBreakingDescription c
            ])
        ]
