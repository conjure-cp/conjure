{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.ToRelation where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpToRelation x = OpToRelation x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpToRelation x)
instance Hashable  x => Hashable  (OpToRelation x)
instance ToJSON    x => ToJSON    (OpToRelation x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpToRelation x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpToRelation x) where
    typeOf p@(OpToRelation x) = do
        tx <- typeOf x
        case tx of
            TypeFunction i j -> return (TypeRelation [i,j])
            _ -> raiseTypeError p

instance Pretty x => DomainOf (OpToRelation x) x where
    domainOf op = na $ "evaluateOp{OpToRelation}:" <++> pretty op

instance EvaluateOp OpToRelation where
    evaluateOp (OpToRelation (ConstantAbstract (AbsLitFunction xs))) =
        return $ ConstantAbstract $ AbsLitRelation $ sortNub [ [a,b] | (a,b) <- xs ]
    evaluateOp op = na $ "evaluateOp{OpToRelation}:" <++> pretty (show op)

instance SimplifyOp OpToRelation where
    simplifyOp _ _ = na "simplifyOp{OpToRelation}"

instance Pretty x => Pretty (OpToRelation x) where
    prettyPrec _ (OpToRelation a) = "toRelation" <> prParens (pretty a)
