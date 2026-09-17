{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module Conjure.Language.Expression.Op.PermutationOrderDelayed where

import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Prelude
import Data.Aeson qualified as JSON -- aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V -- vector

-- first argument: the tuple of permutations to apply (ps)
-- second argument: the value (x)
-- the effect is a subset of: x .<= transform(ps, x)
data OpPermutationOrderDelayed x = OpPermutationOrderDelayed [x] x
  deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance (Serialize x) => Serialize (OpPermutationOrderDelayed x)

instance (Hashable x) => Hashable (OpPermutationOrderDelayed x)

instance (ToJSON x) => ToJSON (OpPermutationOrderDelayed x) where
  toJSON :: (ToJSON x) => OpPermutationOrderDelayed x -> JSON.Value
  toJSON = genericToJSON jsonOptions

instance (FromJSON x) => FromJSON (OpPermutationOrderDelayed x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpPermutationOrderDelayed x) where
  typeOf p@(OpPermutationOrderDelayed perms x) = do
    _tyX <- typeOf x
    forM_ perms $ \pe -> do
      tyP <- typeOf pe
      case tyP of
        TypePermutation {} -> return ()
        _ -> raiseTypeError p
    return TypeBool

instance SimplifyOp OpPermutationOrderDelayed x where
  simplifyOp _ = na "simplifyOp{OpPermutationOrderDelayed}"

instance (Pretty x) => Pretty (OpPermutationOrderDelayed x) where
  prettyPrec _ (OpPermutationOrderDelayed as b) = "permutationOrderDelayed" <> prettyListDoc prParens "," [prettyList prBrackets "," as, pretty b]

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpPermutationOrderDelayed x) where
  varSymBreakingDescription (OpPermutationOrderDelayed xs y) =
    JSON.Object
      $ KM.fromList
        [ ("type", JSON.String "OpPermutationOrderDelayed"),
          ( "children",
            JSON.Array $ V.fromList (map varSymBreakingDescription xs ++ [varSymBreakingDescription y])
          )
        ]
