{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Participants where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpParticipants x = OpParticipants x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpParticipants x)
instance Hashable  x => Hashable  (OpParticipants x)
instance ToJSON    x => ToJSON    (OpParticipants x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpParticipants x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpParticipants x) where
    typeOf inp@(OpParticipants p) = do
        pTy <- typeOf p
        case pTy of
            TypePartition pTyInner -> return (TypeSet pTyInner)
            _ -> raiseTypeError inp

instance Pretty x => DomainOf (OpParticipants x) x where
    domainOf op = na $ "evaluateOp{OpParticipants}:" <++> pretty op

instance EvaluateOp OpParticipants where
    evaluateOp (OpParticipants (ConstantAbstract (AbsLitPartition xss))) =
        return $ ConstantAbstract $ AbsLitSet $ sort $ concat xss
    evaluateOp op = na $ "evaluateOp{OpParticipants}:" <++> pretty (show op)

instance SimplifyOp OpParticipants where
    simplifyOp _ _ = na "simplifyOp{OpParticipants}"

instance Pretty x => Pretty (OpParticipants x) where
    prettyPrec _ (OpParticipants a) = "participants" <> prParens (pretty a)
