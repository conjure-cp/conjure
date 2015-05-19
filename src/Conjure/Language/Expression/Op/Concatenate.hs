{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Concatenate where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpConcatenate x = OpConcatenate x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpConcatenate x)
instance Hashable  x => Hashable  (OpConcatenate x)
instance ToJSON    x => ToJSON    (OpConcatenate x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpConcatenate x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpConcatenate x) where
    typeOf p@(OpConcatenate m) = do
        let flattenType (TypeList inner) = return inner
            flattenType (TypeMatrix _ inner) = return inner
            flattenType _ = raiseTypeError p
        ty <- typeOf m
        case ty of
            TypeList n -> TypeList <$> flattenType n
            TypeMatrix _ n -> TypeList <$> flattenType n
            _ -> raiseTypeError p

instance EvaluateOp OpConcatenate where
    evaluateOp (OpConcatenate (ConstantAbstract (AbsLitMatrix _ xs))) = do
        let flat (ConstantAbstract (AbsLitMatrix _ ys)) = ys
            flat _ = bug "OpConcatenate, not a matrix literal"
        let flattened = concatMap flat xs
        return (ConstantAbstract (AbsLitMatrix
                    (DomainInt [RangeBounded 1 (fromInt (genericLength flattened))])
                    flattened))
    evaluateOp _ = na "evaluateOp{OpConcatenate}"

instance SimplifyOp OpConcatenate x where
    simplifyOp _ = na "simplifyOp{OpConcatenate}"

instance Pretty x => Pretty (OpConcatenate x) where
    prettyPrec _ (OpConcatenate m) = "concatenate" <> prParens (pretty m)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpConcatenate x) where
    varSymBreakingDescription (OpConcatenate a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpConcatenate")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
