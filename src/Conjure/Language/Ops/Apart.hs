{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Apart where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpApart x = OpApart x x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpApart x)
instance Hashable  x => Hashable  (OpApart x)
instance ToJSON    x => ToJSON    (OpApart x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpApart x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpApart x) where
    typeOf inp@(OpApart x y p) = do
        xTy <- typeOf x
        yTy <- typeOf y
        pTy <- typeOf p
        case pTy of
            TypePartition pTyInner | typesUnify [xTy, yTy, pTyInner] -> return TypeBool
            _ -> raiseTypeError inp

instance EvaluateOp OpApart where
    evaluateOp (OpApart x y (ConstantAbstract (AbsLitPartition xss))) =
        return $ ConstantBool $ and
            [ or [ x `elem` xs
                 | xs <- xss
                 ]
            , or [ x `elem` xs
                 | xs <- xss
                 ]
            , or [ not (x `elem` xs && y `elem` xs)
                 | xs <- xss
                 ]
            ]
    evaluateOp op = na $ "evaluateOp{OpApart}:" <++> pretty (show op)

instance SimplifyOp OpApart where
    simplifyOp _ _ = na "simplifyOp{OpApart}"

instance Pretty x => Pretty (OpApart x) where
    prettyPrec _ (OpApart a b c) = "apart" <> prettyList prParens "," [a,b,c]
