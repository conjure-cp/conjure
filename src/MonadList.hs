{-# LANGUAGE FlexibleInstances #-}

module MonadList ( MonadList, option, runListT ) where

import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.List ( ListT(..) )
import Control.Monad.Writer ( WriterT )
import Control.Monad.RWS ( RWST )
import Data.Monoid ( Monoid )


class Monad m => MonadList m where
    option :: [a] -> m a

instance MonadList [] where
    option = id

instance (Monad m) => MonadList (ListT m) where
    option = ListT . return

instance (Monoid w, Monad m) => MonadList (WriterT w (ListT m)) where
    option = lift . option

instance (Monoid w, Monad m) => MonadList (RWST r w s (ListT m)) where
    option = lift . option
