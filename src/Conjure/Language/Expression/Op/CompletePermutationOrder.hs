{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module Conjure.Language.Expression.Op.CompletePermutationOrder where

import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Prelude
import Data.Aeson qualified as JSON -- aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V -- vector

-- first argument: the tuple of permutations to apply (ps)
-- second argument: the value (x)
-- Compare x and its image using exactly the same representation tree.
data OpCompletePermutationOrder x = OpCompletePermutationOrder [x] x
  deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance (Serialize x) => Serialize (OpCompletePermutationOrder x)

instance (Hashable x) => Hashable (OpCompletePermutationOrder x)

instance (ToJSON x) => ToJSON (OpCompletePermutationOrder x) where
  toJSON :: (ToJSON x) => OpCompletePermutationOrder x -> JSON.Value
  toJSON = genericToJSON jsonOptions

instance (FromJSON x) => FromJSON (OpCompletePermutationOrder x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpCompletePermutationOrder x) where
  typeOf p@(OpCompletePermutationOrder perms x) = do
    _tyX <- typeOf x
    forM_ perms $ \pe -> do
      tyP <- typeOf pe
      case tyP of
        TypePermutation {} -> return ()
        _ -> raiseTypeError p
    return TypeBool

instance SimplifyOp OpCompletePermutationOrder x where
  simplifyOp _ = na "simplifyOp{OpCompletePermutationOrder}"

instance (Pretty x) => Pretty (OpCompletePermutationOrder x) where
  prettyPrec _ (OpCompletePermutationOrder as b) = "completePermutationOrder" <> prettyListDoc prParens "," [prettyList prBrackets "," as, pretty b]

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpCompletePermutationOrder x) where
  varSymBreakingDescription (OpCompletePermutationOrder xs y) =
    JSON.Object
      $ KM.fromList
        [ ("type", JSON.String "OpCompletePermutationOrder"),
          ( "children",
            JSON.Array $ V.fromList (map varSymBreakingDescription xs ++ [varSymBreakingDescription y])
          )
        ]
