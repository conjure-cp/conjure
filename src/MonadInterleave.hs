
-- This module defines the `MonadInterleave` type class, and two useful
-- functions to work with an instance of `MonadInterleave`: `interleave` and
-- `yield`.

module MonadInterleave ( MonadInterleave, interleave, yield ) where


import System.IO.Unsafe ( unsafeInterleaveIO )


-- type-class for those monads with a special operation to make them lazier.
class Monad m => MonadInterleave m where

    -- defer the evaluation of the argument computation
    interleave :: m a -> m a


-- replacement for `return`
yield :: MonadInterleave m => a -> m a
yield = interleave . return


-- IO is a natural instance of MonadInterleave
instance MonadInterleave IO where
    interleave = unsafeInterleaveIO
