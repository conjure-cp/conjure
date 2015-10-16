module Conjure.Process.Enumerate ( EnumerateDomain ) where

import Conjure.Prelude

class (Functor m, Applicative m, Monad m) => EnumerateDomain m where liftIO' :: IO a -> m a
