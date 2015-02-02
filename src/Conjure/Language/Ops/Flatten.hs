{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Flatten where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpFlatten x = OpFlatten x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpFlatten x)
instance Hashable  x => Hashable  (OpFlatten x)
instance ToJSON    x => ToJSON    (OpFlatten x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpFlatten x) where parseJSON = genericParseJSON jsonOptions

instance TypeOf x => TypeOf (OpFlatten x) where
    typeOf (OpFlatten m) = do
        let flattenType (TypeList inner) = flattenType inner
            flattenType ty = ty
        TypeList n <- typeOf m
        return (TypeList (flattenType n))

instance EvaluateOp OpFlatten where
    evaluateOp (OpFlatten m) = do
        let flat (ConstantAbstract (AbsLitMatrix _ xs)) = concatMap flat xs
            flat c = [c]
        let flattened = flat m
        return (ConstantAbstract (AbsLitMatrix
                    (DomainInt [RangeBounded 1 (fromInt (length flattened))])
                    flattened))

instance SimplifyOp OpFlatten where
    simplifyOp _ _ = na "simplifyOp{OpFlatten}"

instance Pretty x => Pretty (OpFlatten x) where
    prettyPrec _ (OpFlatten m) = "flatten" <> prParens (pretty m)
