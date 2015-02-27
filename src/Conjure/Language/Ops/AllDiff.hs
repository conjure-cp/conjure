{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Conjure.Language.Ops.AllDiff where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpAllDiff x = OpAllDiff x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpAllDiff x)
instance Hashable  x => Hashable  (OpAllDiff x)
instance ToJSON    x => ToJSON    (OpAllDiff x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpAllDiff x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpAllDiff x) where
    typeOf p@(OpAllDiff x) = do
        ty <- typeOf x
        case ty of
            TypeList{} -> return TypeBool
            TypeMatrix{} -> return TypeBool
            _ -> raiseTypeError p

instance Pretty x => DomainOf (OpAllDiff x) x where
    domainOf op = na $ "evaluateOp{OpAllDiff}:" <++> pretty op

instance EvaluateOp OpAllDiff where
    evaluateOp (OpAllDiff (ConstantAbstract (AbsLitMatrix _ vals))) =
        return $ ConstantBool $ length vals == length (nub vals)
    evaluateOp op = na $ "evaluateOp{OpAllDiff}:" <++> pretty (show op)

instance SimplifyOp OpAllDiff where
    simplifyOp _ _ = na "simplifyOp{OpAllDiff}"

instance Pretty x => Pretty (OpAllDiff x) where
    prettyPrec _ (OpAllDiff a) = "allDiff" <> prParens (pretty a)
