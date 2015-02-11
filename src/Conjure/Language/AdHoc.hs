{-# LANGUAGE MultiParamTypeClasses #-}

module Conjure.Language.AdHoc where

import Conjure.Prelude
import Conjure.Language.Name


class ExpressionLike a where
    fromInt :: Int -> a
    intOut :: MonadFail m => a -> m Int

    fromBool :: Bool -> a
    boolOut :: MonadFail m => a -> m Bool

    fromList :: [a] -> a
    listOut :: MonadFail m => a -> m [a]

class ReferenceContainer a where
    fromName :: Name -> a

class DomainContainer a dom where
    fromDomain :: dom a -> a
    domainOut :: MonadFail m => a -> m (dom a)

class CanBeAnAlias a where
    isAlias :: a -> Maybe a
