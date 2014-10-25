module Conjure.Language.AdHoc where

import Conjure.Prelude
import Conjure.Language.Name


class IntContainer a where
    intOut :: MonadFail m => a -> m Int

class ExpressionLike a where
    fromInt :: Int -> a
    fromBool :: Bool -> a

class ReferenceContainer a where
    fromName :: Name -> a

