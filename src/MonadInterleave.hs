module MonadInterleave ( MonadInterleave, interleave, yield ) where

import Control.Monad.Identity ( Identity )
import System.IO.Unsafe ( unsafeInterleaveIO )

class Monad m => MonadInterleave m where
    interleave :: m a -> m a

yield :: MonadInterleave m => a -> m a
yield = interleave . return

instance MonadInterleave IO where
    interleave = unsafeInterleaveIO

instance MonadInterleave Identity where
    interleave = id
