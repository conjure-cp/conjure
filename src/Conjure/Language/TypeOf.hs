module Conjure.Language.TypeOf
    ( TypeOf(..)
    , TypeCheckerMode(..)
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Type


class TypeOf a where
    typeOf :: (MonadFail m, ?typeCheckerMode :: TypeCheckerMode) => a -> m Type


-- typeOf takes an implicit parameter.

-- This parameter will decide the mode of the type checker.
-- Currently there are two modes: StronglyTyped and RelaxedIntegerTags.
-- StronglyTyped is used for user input.
-- RelaxedIntegerTags is for internal use only and it ignores the integer tags during type checking.

data TypeCheckerMode = StronglyTyped | RelaxedIntegerTags
