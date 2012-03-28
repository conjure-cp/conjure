{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.ListT ( ListT, fromList, runListT ) where

import Control.Applicative ( Applicative, (<*>), pure )
import Control.Monad ( MonadPlus, mzero, mplus, liftM, ap )
import Control.Monad.Error ( MonadError, throwError, catchError )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader ( MonadReader, ask, local )
import Control.Monad.State ( MonadState, get, put )
import Control.Monad.Trans.Class ( MonadTrans, lift )
import Control.Monad.Writer ( MonadWriter, tell, listen, pass )
import Data.Monoid ( Monoid )



-- The monadic list type
data MList' m a = MNil | a `MCons` MList m a
type MList m a  = m (MList' m a)

-- This can be directly used as a monad transformer
newtype ListT m a = ListT { runListT_MList :: MList m a }

-- A "lazy" run function, which only calculates the first solution.
runListT_MList' :: Functor m => ListT m a -> m (Maybe (a, ListT m a))
runListT_MList' (ListT m) = fmap g m
    where
        g MNil = Nothing
        g (x `MCons` xs) = Just (x, ListT xs)

-- In ListT from Control.Monad this one is the data constructor ListT, so sadly, this code can't be a drop-in replacement.
fromList :: Monad m => [a] -> ListT m a
fromList [] = ListT $ return MNil
fromList (x:xs) = ListT . return $ x `MCons` (runListT_MList $ fromList xs)

runListT :: (Functor m, Monad m) => ListT m a -> m [a]
runListT l = do
    res <- runListT_MList' l
    case res of
        Nothing     -> return []
        Just (x,ls) -> do
            xs <- runListT ls
            return (x : xs)


instance Functor m => Functor (ListT m) where
    fmap f (ListT m) = ListT $ fmap (fmap f) m
 
instance Functor m => Functor (MList' m) where
    fmap _ MNil = MNil
    fmap f (x `MCons` xs) = f x `MCons` fmap (fmap f) xs
 
-- Why on earth isn't Monad declared `class Functor m => Monad m'?
-- I assume that a monad is always a functor, so the contexts 
-- get a little larger than actually necessary
instance (Functor m, Monad m) => Monad (ListT m) where
    return x = ListT . return $ x `MCons` return MNil
    m >>= f = joinListT $ fmap f m

instance (Functor m, Monad m) => Applicative (ListT m) where
    pure = return
    (<*>) = ap

instance MonadTrans ListT where
  lift = ListT . liftM (`MCons` return MNil)

instance (Functor m, Monad m) => MonadPlus (ListT m) where
  mzero = fromList []
  (ListT xs) `mplus` (ListT ys) = ListT $ xs `mAppend` ys

-- Implemenation of join
joinListT :: (Functor m, Monad m) => ListT m (ListT m a) -> ListT m a
joinListT (ListT xss) = ListT . joinMList $ fmap (fmap runListT_MList) xss
 
joinMList :: (Functor m, Monad m) => MList m (MList m a) -> MList m a
joinMList = (=<<) joinMList'
 
joinMList' :: (Functor m, Monad m) => MList' m (MList m a) -> MList m a
joinMList' MNil = return MNil
joinMList' (x `MCons` xs) = x `mAppend` joinMList xs
 
mAppend :: (Functor m, Monad m) => MList m a -> MList m a -> MList m a
mAppend xs ys = (`mAppend'` ys) =<< xs
 
mAppend' :: (Functor m, Monad m) => MList' m a -> MList m a -> MList m a
mAppend' MNil           ys = ys
mAppend' (x `MCons` xs) ys = return $ x `MCons` mAppend xs ys
 
-- These things typecheck, but I haven't made sure what they do is sensible.
-- (callCC almost certainly has to be changed in the same way as throwError)
instance (MonadIO m, Functor m) => MonadIO (ListT m) where
    liftIO = lift . liftIO

instance (MonadWriter w m, Functor m, Monoid w) => MonadWriter w (ListT m) where
    tell = lift . tell
    listen _ = error "listen on ListT"
    pass _ = error "pass on ListT"

instance (MonadReader s m, Functor m) => MonadReader s (ListT m) where
    ask     = lift ask
    local f = ListT . local f . runListT_MList

instance (MonadState s m, Functor m) => MonadState s (ListT m) where
    get = lift get
    put = lift . put

instance (MonadError e m, Functor m) => MonadError e (ListT m) where
    throwError = lift . throwError
    (m :: ListT m a) `catchError` h = ListT . deepCatch . runListT_MList $ m
        where
            deepCatch :: MList m a -> MList m a
            deepCatch ml = fmap deepCatch' ml `catchError` \e -> runListT_MList (h e)

            deepCatch' :: MList' m a -> MList' m a
            deepCatch' MNil = MNil 
            deepCatch' (x `MCons` xs) = x `MCons` deepCatch xs
