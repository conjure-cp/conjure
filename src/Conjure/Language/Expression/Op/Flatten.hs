{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Flatten where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpFlatten x = OpFlatten x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpFlatten x)
instance Hashable  x => Hashable  (OpFlatten x)
instance ToJSON    x => ToJSON    (OpFlatten x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpFlatten x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpFlatten x) where
    typeOf p@(OpFlatten m) = do
        let flattenType (TypeList inner) = flattenType inner
            flattenType (TypeMatrix _ inner) = flattenType inner
            flattenType ty = ty
        ty <- typeOf m
        case ty of
            TypeList n -> return (TypeList (flattenType n))
            TypeMatrix _ n -> return (TypeList (flattenType n))
            _ -> raiseTypeError p

instance (Pretty x, TypeOf x) => DomainOf (OpFlatten x) x where
    domainOf op = mkDomainAny ("OpFlatten:" <++> pretty op) <$> typeOf op

instance EvaluateOp OpFlatten where
    evaluateOp (OpFlatten m) = do
        let flat (ConstantAbstract (AbsLitMatrix _ xs)) = concatMap flat xs
            flat c = [c]
        let flattened = flat m
        return (ConstantAbstract (AbsLitMatrix
                    (DomainInt [RangeBounded 1 (fromInt (genericLength flattened))])
                    flattened))

instance SimplifyOp OpFlatten x where
    simplifyOp _ = na "simplifyOp{OpFlatten}"

instance Pretty x => Pretty (OpFlatten x) where
    prettyPrec _ (OpFlatten m) = "flatten" <> prParens (pretty m)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpFlatten x) where
    varSymBreakingDescription (OpFlatten a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpFlatten")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
