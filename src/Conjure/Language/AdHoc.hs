module Conjure.Language.AdHoc where

import Conjure.Prelude
import Conjure.Language.Name


class ExpressionLike a where
    fromInt :: Int -> a
    intOut :: MonadFail m => a -> m Int
    fromBool :: Bool -> a
    boolOut :: MonadFail m => a -> m Bool

class ReferenceContainer a where
    fromName :: Name -> a

class CanBeAnAlias a where
    isAlias :: a -> Maybe a
