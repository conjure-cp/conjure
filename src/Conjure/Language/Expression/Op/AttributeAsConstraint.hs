{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.AttributeAsConstraint where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpAttributeAsConstraint x = OpAttributeAsConstraint x
                                                         AttrName   -- attribute name
                                                         (Maybe x)  -- it's value
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpAttributeAsConstraint x)
instance Hashable  x => Hashable  (OpAttributeAsConstraint x)
instance ToJSON    x => ToJSON    (OpAttributeAsConstraint x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpAttributeAsConstraint x) where parseJSON = genericParseJSON jsonOptions

instance TypeOf x => TypeOf (OpAttributeAsConstraint x) where
    -- TODO
    typeOf OpAttributeAsConstraint{} = return TypeBool

instance Pretty x => DomainOf (OpAttributeAsConstraint x) x where
    domainOf op = na $ "evaluateOp{OpAttributeAsConstraint}:" <++> pretty op

instance EvaluateOp OpAttributeAsConstraint where
    -- TODO
    evaluateOp _ = return (ConstantBool True)

instance SimplifyOp OpAttributeAsConstraint x where
    simplifyOp _ = na "simplifyOp{OpAttributeAsConstraint}"

instance Pretty x => Pretty (OpAttributeAsConstraint x) where
    prettyPrec _ (OpAttributeAsConstraint x attr Nothing   ) = pretty attr <> prParens (pretty x)
    prettyPrec _ (OpAttributeAsConstraint x attr (Just val)) = pretty attr <> prettyList prParens "," [x, val]
