{-# LANGUAGE MultiParamTypeClasses #-}

module Conjure.Language.DomainOf where

-- conjure
import Conjure.Prelude
import Conjure.Language.Domain


class DomainOf st r x a where
    domainOf :: (MonadState st m, MonadFail m) => Proxy r -> a -> m (Domain r x)
