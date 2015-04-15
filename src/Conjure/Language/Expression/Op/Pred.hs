{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Language.Expression.Op.Pred where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpPred x = OpPred x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpPred x)
instance Hashable  x => Hashable  (OpPred x)
instance ToJSON    x => ToJSON    (OpPred x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpPred x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpPred x) where
    typeOf p@(OpPred x) = do
        ty <- typeOf x
        case ty of
            TypeBool{} -> return ty
            TypeInt{}  -> return ty
            TypeEnum{} -> return ty
            _ -> raiseTypeError p

instance (DomainOf x x) => DomainOf (OpPred x) x where
    domainOf (OpPred x) = domainOf x

instance EvaluateOp OpPred where
    evaluateOp p | any isUndef (universeBi p) = return $ mkUndef TypeInt $ "Has undefined children:" <+> pretty p
    evaluateOp (OpPred (ConstantInt x)) = return (ConstantInt (pred x))
    evaluateOp op = na $ "evaluateOp{OpPred}" <+> pretty (show op)

instance SimplifyOp OpPred x where
    simplifyOp _ = na "simplifyOp{OpPred}"

instance (Pretty x, ExpressionLike x) => Pretty (OpPred x) where
    prettyPrec _ (OpPred x) = "pred" <> prParens (pretty x)

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpPred x) where
    varSymBreakingDescription (OpPred a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpPred")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
