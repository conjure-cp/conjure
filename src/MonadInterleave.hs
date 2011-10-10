module MonadInterleave ( MonadInterleave, interleave, yield ) where

import System.IO.Unsafe ( unsafeInterleaveIO )

class Monad m => MonadInterleave m where
    interleave :: m a -> m a

yield :: MonadInterleave m => a -> m a
yield = interleave . return

instance MonadInterleave IO where
    interleave = unsafeInterleaveIO
