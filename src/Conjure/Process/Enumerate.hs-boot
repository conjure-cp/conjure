module Conjure.Process.Enumerate ( EnumerateDomain ) where

import Conjure.Prelude
import Conjure.UserError

class (Functor m, Applicative m, Monad m, MonadUserError m) => EnumerateDomain m where liftIO' :: IO a -> m a
instance EnumerateDomain IO
-- instance EnumerateDomain m => EnumerateDomain (IdentityT m)
-- instance EnumerateDomain m => EnumerateDomain (MaybeT m)
instance EnumerateDomain m => EnumerateDomain (ExceptT m)
-- instance EnumerateDomain m => EnumerateDomain (ReaderT r m)
-- instance (EnumerateDomain m, Monoid w) => EnumerateDomain (WriterT w m)
-- instance EnumerateDomain m => EnumerateDomain (StateT st m)
-- instance EnumerateDomain m => EnumerateDomain (Pipes.Proxy a b c d m)
-- instance EnumerateDomain m => EnumerateDomain (NameGenM m)
-- instance (EnumerateDomain m, MonadFail m) => EnumerateDomain (UserErrorT m)
