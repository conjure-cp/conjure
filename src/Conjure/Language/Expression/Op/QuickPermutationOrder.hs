{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module Conjure.Language.Expression.Op.QuickPermutationOrder where

import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Prelude
import Data.Aeson qualified as JSON -- aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V -- vector


-- first argument: the value (x)
-- second argument: the tuple of permutations to apply (ps)
-- the effect is a subset of: x .<= transform(ps, x)
data OpQuickPermutationOrder x = OpQuickPermutationOrder x x
  deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance (Serialize x) => Serialize (OpQuickPermutationOrder x)

instance (Hashable x) => Hashable (OpQuickPermutationOrder x)

instance (ToJSON x) => ToJSON (OpQuickPermutationOrder x) where toJSON :: ToJSON x => OpQuickPermutationOrder x -> JSON.Value
                                                                toJSON = genericToJSON jsonOptions

instance (FromJSON x) => FromJSON (OpQuickPermutationOrder x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpQuickPermutationOrder x) where
  typeOf p@(OpQuickPermutationOrder x perm) = do
    _tyX <- typeOf x
    case listOut perm of
      Just perms -> do
        forM_ perms $ \ pe -> do
          tyP <- typeOf pe
          case tyP of
            TypePermutation {} -> return ()
            _ -> raiseTypeError p
      _ -> raiseTypeError p
    return TypeBool

instance SimplifyOp OpQuickPermutationOrder x where
  simplifyOp _ = na "simplifyOp{OpQuickPermutationOrder}"

instance (Pretty x) => Pretty (OpQuickPermutationOrder x) where
  prettyPrec _ (OpQuickPermutationOrder a b) = "quickPermutationOrder" <> prettyList prParens "," [a, b]

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpQuickPermutationOrder x) where
  varSymBreakingDescription (OpQuickPermutationOrder x y) =
    JSON.Object
      $ KM.fromList
        [ ("type", JSON.String "OpQuickPermutationOrder"),
          ( "children",
            JSON.Array
              $ V.fromList
                [ varSymBreakingDescription x,
                  varSymBreakingDescription y
                ]
          )
        ]
