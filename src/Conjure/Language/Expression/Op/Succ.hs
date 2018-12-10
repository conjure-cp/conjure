{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

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

instance (TypeOf x, Pretty x) => TypeOf (OpSucc x) where
    typeOf p@(OpSucc x) = do
        ty <- typeOf x
        case ty of
            TypeBool{} -> return ty
            TypeInt NoTag  -> return ty
            TypeInt (TagEnum _)  -> return ty
            TypeEnum{} -> return ty
            _ -> raiseTypeError p

instance EvaluateOp OpSucc where
    evaluateOp p | any isUndef (childrenBi p) =
        return $ mkUndef (TypeInt AnyTag) $ "Has undefined children:" <+> pretty p
    evaluateOp (OpSucc (ConstantBool False)) = return (ConstantBool True)
    evaluateOp (OpSucc (ConstantBool True )) = return (ConstantBool False)          -- undef
    evaluateOp (OpSucc (ConstantInt NoTag x)) = return (ConstantInt NoTag (succ x))
    evaluateOp (OpSucc (ConstantInt (TagEnum t) x))
        = return (ConstantInt (TagEnum t) (succ x))
    evaluateOp op = na $ "evaluateOp{OpSucc}" <+> pretty (show op)

instance SimplifyOp OpSucc x where
    simplifyOp _ = na "simplifyOp{OpSucc}"

instance Pretty x => Pretty (OpSucc x) where
    prettyPrec _ (OpSucc x) = "succ" <> prParens (pretty x)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpSucc x) where
    varSymBreakingDescription (OpSucc a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpSucc")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
