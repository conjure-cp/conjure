{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Range where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpRange x = OpRange x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpRange x)
instance Hashable  x => Hashable  (OpRange x)
instance ToJSON    x => ToJSON    (OpRange x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpRange x) where parseJSON = genericParseJSON jsonOptions

instance (Pretty x, TypeOf x) => TypeOf (OpRange x) where
    typeOf p@(OpRange x) = do
        ty <- typeOf x
        case ty of
            TypeFunction _ a -> return (TypeSet a)
            TypeSequence a   -> return (TypeSet a)
            _                -> raiseTypeError p

instance (Pretty x, DomainOf x x) => DomainOf (OpRange x) x where
    domainOf (OpRange f) = do
        fDom <- domainOf f
        case fDom of
            DomainFunction _ _ _ to -> return $ DomainSet def def to
            _ -> fail "domainOf, OpRange, not a function"

instance EvaluateOp OpRange where
    evaluateOp (OpRange (ConstantAbstract (AbsLitFunction xs))) =
        return (ConstantAbstract (AbsLitSet (sortNub (map snd xs))))
    evaluateOp op = na $ "evaluateOp{OpRange}:" <++> pretty (show op)

instance SimplifyOp OpRange x where
    simplifyOp _ = na "simplifyOp{OpRange}"

instance Pretty x => Pretty (OpRange x) where
    prettyPrec _ (OpRange a) = "range" <> prParens (pretty a)
