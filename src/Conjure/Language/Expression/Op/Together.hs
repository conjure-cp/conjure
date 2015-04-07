{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Together where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


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

instance (Pretty x, TypeOf x) => DomainOf (OpTogether x) x where
    domainOf op = mkDomainAny ("OpTogether:" <++> pretty op) <$> typeOf op

instance EvaluateOp OpTogether where
    evaluateOp (OpTogether x y (ConstantAbstract (AbsLitPartition xss))) =
        return $ ConstantBool $ or
            [ x `elem` xs && y `elem` xs
            | xs <- xss
            ]
    evaluateOp op = na $ "evaluateOp{OpTogether}:" <++> pretty (show op)

instance SimplifyOp OpTogether x where
    simplifyOp _ = na "simplifyOp{OpTogether}"

instance Pretty x => Pretty (OpTogether x) where
    prettyPrec _ (OpTogether a b c) = "together" <> prettyList prParens "," [a,b,c]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpTogether x) where
    varSymBreakingDescription (OpTogether a b c) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpTogether")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            , varSymBreakingDescription c
            ])
        , ("symmetricChildren", JSON.Bool True)
        ]
