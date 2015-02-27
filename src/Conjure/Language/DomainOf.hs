{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Conjure.Language.DomainOf where

-- conjure
import Conjure.Prelude
import Conjure.Language.Domain


class DomainOf a x | a -> x where
    domainOf :: MonadFail m => a -> m (Domain () x)
