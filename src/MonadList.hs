{-# LANGUAGE FlexibleInstances #-}

module MonadList ( MonadList, option, runListT ) where

import Control.Monad.Trans.List ( ListT(..) )
import Data.Monoid ( Monoid )
import Control.Monad.Writer ( WriterT )

import Control.Monad.Trans.Class ( lift )


class Monad m => MonadList m where
    option :: [a] -> m a

instance MonadList [] where
    option = id

instance (Monad m) => MonadList (ListT m) where
    option = ListT . return

instance (Monoid w, Monad m) => MonadList (WriterT w (ListT m)) where
    option = lift . option
