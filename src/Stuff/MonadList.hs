module Stuff.MonadList where

-- conjure
import Conjure.Prelude


-- a monad which contains a 'list of things', rather than a single 'thing' in it.
class MonadList m where
    returns :: (Monad m, MonadList m) => [a] -> m a

instance MonadList [] where
    returns = id
