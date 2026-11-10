{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module Conjure.Language.Expression.Op.PermutationOrderAfterRepresentation where

import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Prelude
import Data.Aeson qualified as JSON -- aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V -- vector

-- first argument: the tuple of permutations to apply (ps)
-- second argument: the value (x)
-- the effect is a subset of: x .<= transform(ps, x)
data OpPermutationOrderAfterRepresentation x = OpPermutationOrderAfterRepresentation [x] x
  deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance (Serialize x) => Serialize (OpPermutationOrderAfterRepresentation x)

instance (Hashable x) => Hashable (OpPermutationOrderAfterRepresentation x)

instance (ToJSON x) => ToJSON (OpPermutationOrderAfterRepresentation x) where
  toJSON :: (ToJSON x) => OpPermutationOrderAfterRepresentation x -> JSON.Value
  toJSON = genericToJSON jsonOptions

instance (FromJSON x) => FromJSON (OpPermutationOrderAfterRepresentation x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, ExpressionLike x) => TypeOf (OpPermutationOrderAfterRepresentation x) where
  typeOf p@(OpPermutationOrderAfterRepresentation perms x) = do
    _tyX <- typeOf x
    forM_ perms $ \pe -> do
      tyP <- typeOf pe
      case tyP of
        TypePermutation {} -> return ()
        _ -> raiseTypeError p
    return TypeBool

instance SimplifyOp OpPermutationOrderAfterRepresentation x where
  simplifyOp _ = na "simplifyOp{OpPermutationOrderAfterRepresentation}"

instance (Pretty x) => Pretty (OpPermutationOrderAfterRepresentation x) where
  prettyPrec _ (OpPermutationOrderAfterRepresentation as b) = "permutationOrderAfterRepresentation" <> prettyListDoc prParens "," [prettyList prBrackets "," as, pretty b]

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpPermutationOrderAfterRepresentation x) where
  varSymBreakingDescription (OpPermutationOrderAfterRepresentation xs y) =
    JSON.Object
      $ KM.fromList
        [ ("type", JSON.String "OpPermutationOrderAfterRepresentation"),
          ( "children",
            JSON.Array $ V.fromList (map varSymBreakingDescription xs ++ [varSymBreakingDescription y])
          )
        ]
