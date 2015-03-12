{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Inverse where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpInverse x = OpInverse x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpInverse x)
instance Hashable  x => Hashable  (OpInverse x)
instance ToJSON    x => ToJSON    (OpInverse x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpInverse x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpInverse x) where
    typeOf p@(OpInverse f g) = do
        TypeFunction fFrom fTo <- typeOf f
        TypeFunction gFrom gTo <- typeOf g
        if typesUnify [fFrom, gTo] && typesUnify [fTo, gFrom]
            then return TypeBool
            else raiseTypeError p

instance DomainOf (OpInverse x) x where
    domainOf _ = return DomainBool

instance EvaluateOp OpInverse where
    evaluateOp (OpInverse (ConstantAbstract (AbsLitFunction xs)) (ConstantAbstract (AbsLitFunction ys))) =
        return $ ConstantBool $ and $ concat [ [ (j,i) `elem` ys | (i,j) <- xs ]
                                             , [ (j,i) `elem` xs | (i,j) <- ys ]
                                             ]
    evaluateOp op = na $ "evaluateOp{OpInverse}:" <++> pretty (show op)

instance SimplifyOp OpInverse x where
    simplifyOp _ = na "simplifyOp{OpInverse}"

instance Pretty x => Pretty (OpInverse x) where
    prettyPrec _ (OpInverse a b) = "inverse" <> prettyList prParens "," [a,b]
