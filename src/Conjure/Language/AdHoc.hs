module Conjure.Language.AdHoc where

import Conjure.Prelude
import Conjure.Language.Type
import Conjure.Language.Name

-- aeson
import qualified Data.Aeson.Types as JSON ( Value )


class ExpressionLike a where
    fromInt :: Integer -> a
    fromIntWithTag :: Integer -> IntTag -> a
    intOut :: MonadFail m => Doc -> a -> m Integer

    fromBool :: Bool -> a
    boolOut :: MonadFail m => a -> m Bool

    fromList :: [a] -> a
    listOut :: MonadFail m => a -> m [a]

class ReferenceContainer a where
    fromName :: Name -> a
    nameOut :: MonadFail m => a -> m Name

class DomainContainer a dom where
    fromDomain :: dom a -> a
    domainOut :: MonadFail m => a -> m (dom a)

class CanBeAnAlias a where
    isAlias :: a -> Maybe a

class VarSymBreakingDescription a where
    varSymBreakingDescription :: a -> JSON.Value

class (:<) a b where
    inject :: a -> b
    project :: MonadFail m => b -> m a
