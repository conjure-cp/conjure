{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.Defined where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpDefined x = OpDefined x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpDefined x)
instance Hashable  x => Hashable  (OpDefined x)
instance ToJSON    x => ToJSON    (OpDefined x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDefined x) where parseJSON = genericParseJSON jsonOptions

instance (Pretty x, TypeOf x) => TypeOf (OpDefined x) where
    typeOf p@(OpDefined x) = do
        ty <- typeOf x
        case ty of
            TypeFunction a _ -> return (TypeSet a)
            TypeSequence _   -> return (TypeSet TypeInt)
            _                -> raiseTypeError p

instance EvaluateOp OpDefined where
    evaluateOp p | any isUndef (childrenBi p) = do
        ty <- typeOf p
        return $ mkUndef ty $ "Has undefined children:" <+> pretty p
    evaluateOp (OpDefined (viewConstantFunction -> Just xs)) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub $ map fst xs
    evaluateOp op = na $ "evaluateOp{OpDefined}:" <++> pretty (show op)

instance SimplifyOp OpDefined x where
    simplifyOp _ = na "simplifyOp{OpDefined}"

instance Pretty x => Pretty (OpDefined x) where
    prettyPrec _ (OpDefined a) = "defined" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpDefined x) where
    varSymBreakingDescription (OpDefined a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpDefined")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
