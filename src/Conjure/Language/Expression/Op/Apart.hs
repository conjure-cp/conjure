{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Apart where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


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

instance (Pretty x, TypeOf x) => DomainOf (OpApart x) x where
    domainOf op = mkDomainAny ("OpApart:" <++> pretty op) <$> typeOf op

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

instance SimplifyOp OpApart x where
    simplifyOp _ = na "simplifyOp{OpApart}"

instance Pretty x => Pretty (OpApart x) where
    prettyPrec _ (OpApart a b c) = "apart" <> prettyList prParens "," [a,b,c]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpApart x) where
    varSymBreakingDescription (OpApart a b c) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpApart")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            , varSymBreakingDescription c
            ])
        ]
