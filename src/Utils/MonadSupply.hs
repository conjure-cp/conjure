{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Support for computations which consume values from a (possibly infinite)
-- supply. See http://www.haskell.org/haskellwiki/New_monads/MonadSupply for
-- details.
module Utils.MonadSupply
    ( MonadSupply
    , supply
    , supplies
    , Supply
    , evalSupply
    , runSupply
    , SupplyT
    , evalSupplyT
    , runSupplyT
    ) where

import Control.Monad ( replicateM )
import Control.Monad.Error ( MonadError(..), Error, ErrorT(..) )
import Control.Monad.Identity ( Identity, runIdentity )
import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.Reader ( ReaderT )
import Control.Monad.State ( MonadState, StateT(..), evalStateT, runStateT, get, put )
import Control.Monad.Trans ( MonadTrans, lift )
import Control.Monad.Writer ( MonadWriter(..), WriterT )
import Data.Monoid ( Monoid, mempty, mappend )

import Utils.MonadList ( ListT )


class Monad m => MonadSupply s m | m -> s where
  supply :: m s

-- | Supply monad transformer.
newtype SupplyT s m a = SupplyT (StateT [s] m a)
    deriving (Functor, Monad, MonadTrans, MonadIO)

-- | Supply monad. 
newtype Supply s a = Supply (SupplyT s Identity a)
    deriving (Functor, Monad, MonadSupply s)

instance Monad m => MonadSupply s (SupplyT s m) where
    supply = SupplyT $ do
        (x:xs) <- get
        put xs
        return x


instance MonadState st m => MonadState st (SupplyT su m) where
    get = lift get
    put = lift . put

instance (Error e, MonadError e m) => MonadError e (SupplyT s m) where
    throwError = lift . throwError
    catchError m h = SupplyT $ StateT $ \ s -> runSupplyT m s `catchError` \ e -> runSupplyT (h e) s

instance (Monoid w, MonadWriter w m) => MonadWriter w (SupplyT s m) where
    tell = lift . tell
    listen m = SupplyT $ StateT $ \ s -> do
        ~((a,s'),w) <- listen (runSupplyT m s)
        return ((a,w),s')
    pass m = SupplyT $ StateT $ \ s -> pass $ do
        ~((a, f), s') <- runSupplyT m s
        return ((a, s'), f)



instance MonadSupply s m => MonadSupply s (StateT st m) where
    supply = lift supply

instance MonadSupply s m => MonadSupply s (ReaderT r m) where
    supply = lift supply

instance (Monoid w, MonadSupply s m) => MonadSupply s (WriterT w m) where
    supply = lift supply

instance (Functor m, MonadSupply s m) => MonadSupply s (ListT m) where
    supply = lift supply

instance (Error e, MonadSupply s m) => MonadSupply s (ErrorT e m) where
    supply = lift supply



-- | Monoid instance for the supply monad. Actually any monad/monoid pair gives
-- rise to this monoid instance, but we can't write it like that because it
-- would conflict with existing instances provided by Data.Monoid.
instance (Monoid a) => Monoid (Supply s a) where
    mempty = return mempty
    m1 `mappend` m2 = do
        x1 <- m1
        x2 <- m2
        return (x1 `mappend` x2)

-- | Get n supplies.
supplies :: MonadSupply s m => Int -> m [s]
supplies n = replicateM n supply

evalSupplyT :: Monad m => SupplyT s m a -> [s] -> m a
evalSupplyT (SupplyT s) = evalStateT s

evalSupply :: Supply s a -> [s] -> a
evalSupply (Supply s) = runIdentity . evalSupplyT s

runSupplyT :: Monad m => SupplyT s m a -> [s] -> m (a,[s])
runSupplyT (SupplyT s) = runStateT s

runSupply :: Supply s a -> [s] -> (a,[s])
runSupply (Supply s) = runIdentity . runSupplyT s
