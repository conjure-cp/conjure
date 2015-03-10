{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.ToSet where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpToSet x = OpToSet x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpToSet x)
instance Hashable  x => Hashable  (OpToSet x)
instance ToJSON    x => ToJSON    (OpToSet x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpToSet x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpToSet x) where
    typeOf p@(OpToSet x) = do
        tx <- typeOf x
        case tx of
            TypeRelation is  -> return (TypeSet (TypeTuple is))
            TypeMSet i       -> return (TypeSet i)
            TypeFunction i j -> return (TypeSet (TypeTuple [i,j]))
            TypeMatrix _ i   -> return (TypeSet i)
            TypeList i       -> return (TypeSet i)
            _ -> raiseTypeError p

instance (Pretty x, TypeOf x) => DomainOf (OpToSet x) x where
    domainOf op = mkDomainAny ("OpToSet:" <++> pretty op) <$> typeOf op

instance EvaluateOp OpToSet where
    evaluateOp (OpToSet (ConstantAbstract (AbsLitMatrix _ xs))) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub xs
    evaluateOp (OpToSet (ConstantAbstract (AbsLitSet xs))) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub xs
    evaluateOp (OpToSet (ConstantAbstract (AbsLitMSet xs))) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub xs
    evaluateOp (OpToSet (ConstantAbstract (AbsLitFunction xs))) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub [ConstantAbstract (AbsLitTuple [a,b]) | (a,b) <- xs]
    evaluateOp (OpToSet (ConstantAbstract (AbsLitRelation xs))) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub $ map (ConstantAbstract . AbsLitTuple) xs
    evaluateOp op = na $ "evaluateOp{OpToSet}:" <++> pretty (show op)

instance SimplifyOp OpToSet x where
    simplifyOp _ = na "simplifyOp{OpToSet}"

instance Pretty x => Pretty (OpToSet x) where
    prettyPrec _ (OpToSet a) = "toSet" <> prParens (pretty a)
