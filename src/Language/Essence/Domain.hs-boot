{-# LANGUAGE FlexibleContexts #-}

module Language.Essence.Domain where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Data.Generics ( Data )
import Data.Typeable ( Typeable )
-- import GHC.Generics ( Generic )
import Test.QuickCheck ( Arbitrary )

import Has
import GenericOps.Core
import ParsePrint
import PrintUtils

import {-# SOURCE #-} Language.Essence.Type



class DomainOf a where
    domainOf ::
        ( Applicative m
        , Has st BindingsMap
        , Has st [GNode]
        , MonadError Doc m
        , MonadState st m
        , MonadWriter [Doc] m
        ) => a -> m Domain

data Domain

instance Eq Domain
instance Ord Domain
instance Read Domain
instance Show Domain
instance Data Domain
instance Typeable Domain
-- instance Generic Domain

instance NodeTag Domain
instance Hole Domain
instance GPlate Domain
instance MatchBind Domain
instance ParsePrint Domain
instance Arbitrary Domain
instance TypeOf Domain
instance DomainOf Domain
