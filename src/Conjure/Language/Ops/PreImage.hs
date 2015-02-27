{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.PreImage where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpPreImage x = OpPreImage x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpPreImage x)
instance Hashable  x => Hashable  (OpPreImage x)
instance ToJSON    x => ToJSON    (OpPreImage x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpPreImage x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpPreImage x) where
    typeOf p@(OpPreImage f x) = do
        TypeFunction from to <- typeOf f
        xTy <- typeOf x
        if typesUnify [xTy, to]
            then return (TypeSet from)
            else raiseTypeError p

instance Pretty x => DomainOf (OpPreImage x) x where
    domainOf op = na $ "evaluateOp{OpPreImage}:" <++> pretty op

instance EvaluateOp OpPreImage where
    evaluateOp (OpPreImage (ConstantAbstract (AbsLitFunction xs)) a) =
        return $ ConstantAbstract $ AbsLitSet [ x | (x,y) <- xs, a == y ]
    evaluateOp op = na $ "evaluateOp{OpPreImage}:" <++> pretty (show op)

instance SimplifyOp OpPreImage where
    simplifyOp _ _ = na "simplifyOp{OpPreImage}"

instance Pretty x => Pretty (OpPreImage x) where
    prettyPrec _ (OpPreImage a b) = "preImage" <> prettyList prParens "," [a,b]
