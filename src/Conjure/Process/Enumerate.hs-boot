module Conjure.Process.Enumerate ( EnumerateDomain ) where

import Conjure.Prelude
import Conjure.UserError

class (Functor m, Applicative m, Monad m, MonadUserError m) => EnumerateDomain m where liftIO' :: IO a -> m a
