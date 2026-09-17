{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module Conjure.Language.Expression.Op.ApplySymmetries where

import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Prelude
import Data.Aeson qualified as JSON -- aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V -- vector

-- Quick flag, ordered values, and a parameter matrix of permutation tuples.
data OpApplySymmetries x = OpApplySymmetries Bool x x
  deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance (Serialize x) => Serialize (OpApplySymmetries x)

instance (Hashable x) => Hashable (OpApplySymmetries x)

instance (ToJSON x) => ToJSON (OpApplySymmetries x) where
  toJSON :: (ToJSON x) => OpApplySymmetries x -> JSON.Value
  toJSON = genericToJSON jsonOptions

instance (FromJSON x) => FromJSON (OpApplySymmetries x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpApplySymmetries x) where
  typeOf p@(OpApplySymmetries _ values symmetries) = do
    tv <- typeOf values
    ts <- typeOf symmetries
    case tv of
      TypeTuple _ -> return ()
      _ -> raiseTypeError $ "applySymmetries expects a tuple of values:" <+> pretty p
    entries <- case ts of
      TypeMatrix TypeInt{} (TypeTuple xs) -> return xs
      TypeList (TypeTuple xs) -> return xs
      _ -> raiseTypeError $ "applySymmetries expects a matrix of permutation tuples:" <+> pretty p
    domains <- forM entries $ \t -> case t of
      TypePermutation d -> return d
      _ -> raiseTypeError $ "applySymmetries entry is not a permutation:" <+> pretty p
    unless (length domains == length (nub domains)) $
      raiseTypeError $ "applySymmetries has multiple permutations for the same type:" <+> pretty p
    return TypeBool

instance SimplifyOp OpApplySymmetries x where
  simplifyOp _ = na "simplifyOp{OpApplySymmetries}"

instance Pretty x => Pretty (OpApplySymmetries x) where
  prettyPrec _ (OpApplySymmetries quick values symmetries) =
    (if quick then "applySymmetriesQuick" else "applySymmetries") <>
      prettyList prParens "," [values, symmetries]

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpApplySymmetries x) where
  varSymBreakingDescription (OpApplySymmetries quick values symmetries) = JSON.Object $ KM.fromList
    [ ("type", JSON.String (if quick then "OpApplySymmetriesQuick" else "OpApplySymmetries"))
    , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription [values, symmetries])
    ]
