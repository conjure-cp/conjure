{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Defined where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpDefined x = OpDefined x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpDefined x)
instance Hashable  x => Hashable  (OpDefined x)
instance ToJSON    x => ToJSON    (OpDefined x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDefined x) where parseJSON = genericParseJSON jsonOptions

instance TypeOf x => TypeOf (OpDefined x) where
    typeOf (OpDefined x) = do
        TypeFunction a _ <- typeOf x
        return (TypeSet a)

instance (Pretty x, DomainOf x x) => DomainOf (OpDefined x) x where
    domainOf (OpDefined f) = do
        fDom <- domainOf f
        case fDom of
            DomainFunction _ _ fr _ -> return $ DomainSet def def fr
            _ -> fail "domainOf, OpDefined, not a function"

instance EvaluateOp OpDefined where
    evaluateOp (OpDefined (ConstantAbstract (AbsLitFunction xs))) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub $ map fst xs
    evaluateOp op = na $ "evaluateOp{OpDefined}:" <++> pretty (show op)

instance SimplifyOp OpDefined where
    simplifyOp _ _ = na "simplifyOp{OpDefined}"

instance Pretty x => Pretty (OpDefined x) where
    prettyPrec _ (OpDefined a) = "defined" <> prParens (pretty a)
