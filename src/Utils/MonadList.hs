{-# LANGUAGE FlexibleInstances #-}

module Utils.MonadList ( MonadList, option, ListT, fromList, runListT ) where


import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Cont ( ContT )
import Control.Monad.Trans.Error ( ErrorT, Error )
import Control.Monad.Trans.Identity ( IdentityT )
import Control.Monad.Trans.Maybe ( MaybeT )
import Control.Monad.Trans.Reader ( ReaderT )
import Control.Monad.Trans.Writer.Lazy as Lazy ( WriterT )
import Control.Monad.Trans.Writer.Strict as Strict ( WriterT )
import Data.Monoid ( Monoid )
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS ( RWST )
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS ( RWST )
import qualified Control.Monad.Trans.State.Lazy as Lazy ( StateT )
import qualified Control.Monad.Trans.State.Strict as Strict ( StateT )

import Utils.ListT ( ListT, fromList, runListT )

-- import Control.Monad.IO.Class
-- import Control.Monad.State
-- import Control.Applicative


class Monad m => MonadList m where
    option :: [a] -> m a

instance MonadList [] where
    option = id

instance (Functor m, Monad m) => MonadList (ListT m) where
    option = fromList

instance (MonadList m, Error e)  => MonadList (ErrorT e m)              where option = lift . option
instance (MonadList m, Monoid w) => MonadList (Lazy.WriterT w m)        where option = lift . option
instance (MonadList m, Monoid w) => MonadList (LazyRWS.RWST r w s m)    where option = lift . option
instance (MonadList m, Monoid w) => MonadList (Strict.WriterT w m)      where option = lift . option
instance (MonadList m, Monoid w) => MonadList (StrictRWS.RWST r w s m)  where option = lift . option
instance MonadList m             => MonadList (ContT r m)               where option = lift . option
instance MonadList m             => MonadList (IdentityT m)             where option = lift . option
instance MonadList m             => MonadList (Lazy.StateT   s m)       where option = lift . option
instance MonadList m             => MonadList (MaybeT m)                where option = lift . option
instance MonadList m             => MonadList (ReaderT r m)             where option = lift . option
instance MonadList m             => MonadList (Strict.StateT s m)       where option = lift . option


-- test :: IO ()
-- test = mapM_ print =<< (take 3 <$> runListT list)
-- 
-- list :: (MonadList m, MonadIO m) => m Int
-- list = do
--     is <- forM [0..9] $ \ i -> do
--         liftIO $ putStr " -- "
--         liftIO $ print i
--         return i
--     option is
-- 
-- 
-- summ :: (MonadList m, MonadIO m) => m [Int] -> m Int
-- summ mls = do
--     ls <- mls
--     flip execStateT 0 $ forM_ ls $ \ i -> do
--             liftIO $ print i
--             modify (+i)
-- 
-- 
-- 
-- 
-- 
