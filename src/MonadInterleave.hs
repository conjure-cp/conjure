
-- This module defines the `MonadInterleave` type class, and two useful
-- functions to work with an instance of `MonadInterleave`: `interleave` and
-- `yield`.

module MonadInterleave ( MonadInterleave, interleave, yield ) where


import Control.Monad.Trans.List ( ListT(..) )
import Control.Monad.Identity ( Identity )
import Control.Monad.RWS
import Control.Monad.Error ( Error, ErrorT )
import System.IO.Unsafe ( unsafeInterleaveIO )


-- type-class for those monads with a special operation to make them lazier.
class Monad m => MonadInterleave m where

    -- defer the evaluation of the argument computation
    interleave :: m a -> m a
    interleave = id


-- replacement for `return`
yield :: MonadInterleave m => a -> m a
yield = interleave . return


-- IO is a natural instance of MonadInterleave
instance MonadInterleave IO where
    interleave = unsafeInterleaveIO

-- Identity doesn't need any special care
instance MonadInterleave Identity

instance (MonadInterleave m, Error e) => MonadInterleave (ErrorT e m)

instance  (MonadInterleave m) => MonadInterleave (ListT m)

instance (MonadInterleave m, Monoid w) => MonadInterleave (RWST r w s m)
