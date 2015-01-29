module Conjure.Language.TypeOf where

-- conjure
import Conjure.Prelude
import Conjure.Language.Type


class TypeOf a where
    typeOf :: MonadFail m => a -> m Type
