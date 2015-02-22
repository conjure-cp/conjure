{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Active where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpActive x = OpActive x Name
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpActive x)
instance Hashable  x => Hashable  (OpActive x)
instance ToJSON    x => ToJSON    (OpActive x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpActive x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpActive x) where
    typeOf p@(OpActive v n) = do
        ty <- typeOf v
        case ty of
            TypeVariant ts ->
                if elem n (map fst ts)
                    then return TypeBool
                    else raiseTypeError $ vcat [ pretty p
                                               , "The field name isn't a part of the variant type."
                                               , pretty ty
                                               ]
            _ -> raiseTypeError $ vcat [ pretty p
                                       , "First argument has to be a variant type."
                                       , pretty ty
                                       ]

instance EvaluateOp OpActive where
    evaluateOp (OpActive (ConstantAbstract (AbsLitVariant _ n1 _)) n2) = return $ fromBool $ n1 == n2
    evaluateOp op = na $ "evaluateOp{OpActive}:" <++> pretty (show op)

instance SimplifyOp OpActive where
    simplifyOp _ _ = na "simplifyOp{OpActive}"

instance Pretty x => Pretty (OpActive x) where
    prettyPrec _ (OpActive a b) = "active" <> prettyList prParens "," [pretty a, pretty b]
