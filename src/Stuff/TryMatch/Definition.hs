{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Stuff.TryMatch.Definition where

import Control.Applicative ( Applicative )
import Control.Monad.Error ( MonadError, Error(..) )
import Text.PrettyPrint ( Doc )

newtype TryMatch a = TryMatch (Either Doc a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError Doc
             )

instance Error Doc where
