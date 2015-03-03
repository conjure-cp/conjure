{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.ToMSet where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpToMSet x = OpToMSet x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpToMSet x)
instance Hashable  x => Hashable  (OpToMSet x)
instance ToJSON    x => ToJSON    (OpToMSet x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpToMSet x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpToMSet x) where
    typeOf p@(OpToMSet x) = do
        tx <- typeOf x
        case tx of
            TypeRelation is  -> return (TypeMSet (TypeTuple is))
            TypeSet i        -> return (TypeMSet i)
            TypeFunction i j -> return (TypeMSet (TypeTuple [i,j]))
            _ -> raiseTypeError p

instance Pretty x => DomainOf (OpToMSet x) x where
    domainOf op = na $ "evaluateOp{OpToMSet}:" <++> pretty op

instance EvaluateOp OpToMSet where
    evaluateOp (OpToMSet (ConstantAbstract (AbsLitSet xs))) =
        return $ ConstantAbstract $ AbsLitMSet xs
    evaluateOp (OpToMSet (ConstantAbstract (AbsLitMSet xs))) =
        return $ ConstantAbstract $ AbsLitMSet xs
    evaluateOp (OpToMSet (ConstantAbstract (AbsLitFunction xs))) =
        return $ ConstantAbstract $ AbsLitMSet [ConstantAbstract (AbsLitTuple [a,b]) | (a,b) <- xs]
    evaluateOp (OpToMSet (ConstantAbstract (AbsLitRelation xs))) =
        return $ ConstantAbstract $ AbsLitMSet $ map (ConstantAbstract . AbsLitTuple) xs
    evaluateOp op = na $ "evaluateOp{OpToMSet}:" <++> pretty (show op)

instance SimplifyOp OpToMSet x where
    simplifyOp _ = na "simplifyOp{OpToMSet}"

instance Pretty x => Pretty (OpToMSet x) where
    prettyPrec _ (OpToMSet a) = "toMSet" <> prParens (pretty a)
