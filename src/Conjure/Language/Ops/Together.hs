{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Together where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpTogether x = OpTogether x x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpTogether x)
instance Hashable  x => Hashable  (OpTogether x)
instance ToJSON    x => ToJSON    (OpTogether x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpTogether x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpTogether x) where
    typeOf inp@(OpTogether x y p) = do
        xTy <- typeOf x
        yTy <- typeOf y
        pTy <- typeOf p
        case pTy of
            TypePartition pTyInner | typesUnify [xTy, yTy, pTyInner] -> return TypeBool
            _ -> raiseTypeError inp

instance Pretty x => DomainOf (OpTogether x) x where
    domainOf op = na $ "evaluateOp{OpTogether}:" <++> pretty op

instance EvaluateOp OpTogether where
    evaluateOp (OpTogether x y (ConstantAbstract (AbsLitPartition xss))) =
        return $ ConstantBool $ or
            [ x `elem` xs && y `elem` xs
            | xs <- xss
            ]
    evaluateOp op = na $ "evaluateOp{OpTogether}:" <++> pretty (show op)

instance SimplifyOp OpTogether where
    simplifyOp _ _ = na "simplifyOp{OpTogether}"

instance Pretty x => Pretty (OpTogether x) where
    prettyPrec _ (OpTogether a b c) = "together" <> prettyList prParens "," [a,b,c]
