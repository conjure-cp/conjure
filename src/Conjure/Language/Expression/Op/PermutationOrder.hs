{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module Conjure.Language.Expression.Op.PermutationOrder where

import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Prelude
import Data.Aeson qualified as JSON -- aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V -- vector

-- first argument: the tuple of permutations to apply (ps)
-- second argument: the value (x)
-- Compare x and its image using exactly the same representation tree.
data OpPermutationOrder x = OpPermutationOrder [x] x
  deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance (Serialize x) => Serialize (OpPermutationOrder x)

instance (Hashable x) => Hashable (OpPermutationOrder x)

instance (ToJSON x) => ToJSON (OpPermutationOrder x) where
  toJSON :: (ToJSON x) => OpPermutationOrder x -> JSON.Value
  toJSON = genericToJSON jsonOptions

instance (FromJSON x) => FromJSON (OpPermutationOrder x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpPermutationOrder x) where
  typeOf p@(OpPermutationOrder perms x) = do
    _tyX <- typeOf x
    forM_ perms $ \pe -> do
      tyP <- typeOf pe
      case tyP of
        TypePermutation {} -> return ()
        _ -> raiseTypeError p
    return TypeBool

instance SimplifyOp OpPermutationOrder x where
  simplifyOp _ = na "simplifyOp{OpPermutationOrder}"

instance (Pretty x) => Pretty (OpPermutationOrder x) where
  prettyPrec _ (OpPermutationOrder as b) = "permutationOrder" <> prettyListDoc prParens "," [prettyList prBrackets "," as, pretty b]

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpPermutationOrder x) where
  varSymBreakingDescription (OpPermutationOrder xs y) =
    JSON.Object
      $ KM.fromList
        [ ("type", JSON.String "OpPermutationOrder"),
          ( "children",
            JSON.Array $ V.fromList (map varSymBreakingDescription xs ++ [varSymBreakingDescription y])
          )
        ]
