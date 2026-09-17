{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module Conjure.Language.Expression.Op.PermutationOrderEager where

import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Prelude
import Data.Aeson qualified as JSON -- aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V -- vector

-- first argument: the tuple of permutations to apply (ps)
-- second argument: the value (x)
-- Compare x and its image using exactly the same representation tree.
data OpPermutationOrderEager x = OpPermutationOrderEager [x] x
  deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance (Serialize x) => Serialize (OpPermutationOrderEager x)

instance (Hashable x) => Hashable (OpPermutationOrderEager x)

instance (ToJSON x) => ToJSON (OpPermutationOrderEager x) where
  toJSON :: (ToJSON x) => OpPermutationOrderEager x -> JSON.Value
  toJSON = genericToJSON jsonOptions

instance (FromJSON x) => FromJSON (OpPermutationOrderEager x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpPermutationOrderEager x) where
  typeOf p@(OpPermutationOrderEager perms x) = do
    _tyX <- typeOf x
    forM_ perms $ \pe -> do
      tyP <- typeOf pe
      case tyP of
        TypePermutation {} -> return ()
        _ -> raiseTypeError p
    return TypeBool

instance SimplifyOp OpPermutationOrderEager x where
  simplifyOp _ = na "simplifyOp{OpPermutationOrderEager}"

instance (Pretty x) => Pretty (OpPermutationOrderEager x) where
  prettyPrec _ (OpPermutationOrderEager as b) = "permutationOrderEager" <> prettyListDoc prParens "," [prettyList prBrackets "," as, pretty b]

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpPermutationOrderEager x) where
  varSymBreakingDescription (OpPermutationOrderEager xs y) =
    JSON.Object
      $ KM.fromList
        [ ("type", JSON.String "OpPermutationOrderEager"),
          ( "children",
            JSON.Array $ V.fromList (map varSymBreakingDescription xs ++ [varSymBreakingDescription y])
          )
        ]
