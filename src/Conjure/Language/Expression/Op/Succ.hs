{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Language.Expression.Op.Succ where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpSucc x = OpSucc x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSucc x)
instance Hashable  x => Hashable  (OpSucc x)
instance ToJSON    x => ToJSON    (OpSucc x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSucc x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpSucc x) where
    typeOf p@(OpSucc x) = do
        ty <- typeOf x
        case ty of
            TypeBool{} -> return ty
            TypeInt{}  -> return ty
            TypeEnum{} -> return ty
            _ -> raiseTypeError p

instance (DomainOf x x) => DomainOf (OpSucc x) x where
    domainOf (OpSucc x) = domainOf x

instance EvaluateOp OpSucc where
    evaluateOp p | any isUndef (universeBi p) = return $ mkUndef TypeInt $ "Has undefined children:" <+> pretty p
    evaluateOp (OpSucc (ConstantInt x)) = return (ConstantInt (pred x))
    evaluateOp op = na $ "evaluateOp{OpSucc}" <+> pretty (show op)

instance SimplifyOp OpSucc x where
    simplifyOp _ = na "simplifyOp{OpSucc}"

instance (Pretty x, ExpressionLike x) => Pretty (OpSucc x) where
    prettyPrec _ (OpSucc x) = "pred" <> prParens (pretty x)

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpSucc x) where
    varSymBreakingDescription (OpSucc a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpSucc")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
