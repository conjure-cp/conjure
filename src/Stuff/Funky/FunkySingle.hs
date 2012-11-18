{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Stuff.Funky.FunkySingle where

import Control.Applicative       ( Applicative(..) )
import Control.Monad             ( ap )
import Control.Monad.Base        ( MonadBase(..), liftBaseDefault )
import Control.Monad.Error       ( MonadError(..) )
import Control.Monad.Identity    ( Identity )
import Control.Monad.IO.Class    ( MonadIO, liftIO )
import Control.Monad.State       ( MonadState(..) )
import Control.Monad.Trans.Class ( MonadTrans, lift )


newtype FunkySingle st err m a = FunkySingle (st -> m (Either err a, st))

runFunkySingle :: Monad m => st -> FunkySingle st err m a -> m (Either err a, st)
runFunkySingle st (FunkySingle f) = f st


instance Monad m => Functor (FunkySingle st err m) where
    {-# SPECIALIZE instance Functor (FunkySingle st err Identity) #-}
    {-# INLINE fmap #-}
    fmap f (FunkySingle g) = {-# SCC "FSfmap" #-} FunkySingle $ \ st -> do
        (mx, st') <- g st
        return $ case mx of
            Left  x -> (Left  x    , st')
            Right x -> (Right (f x), st')

instance Monad m => Applicative (FunkySingle st err m) where
    {-# SPECIALIZE instance Applicative (FunkySingle st err Identity) #-}
    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}
    pure x = FunkySingle $ \ st -> return (Right x, st)
    (<*>) = ap

instance Monad m => Monad (FunkySingle st err m) where
    {-# SPECIALIZE instance Monad (FunkySingle st err Identity) #-}
    {-# INLINE fail #-}
    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    fail = error
    return = pure
    FunkySingle g >>= f = {-# SCC "FSbind" #-} FunkySingle $ \ st -> do
        (mx, st') <- g st
        case mx of
            Left  x -> return (Left x, st')
            Right x -> runFunkySingle st' (f x)

instance Monad m => MonadError err (FunkySingle st err m) where
    {-# INLINE throwError #-}
    {-# INLINE catchError #-}
    throwError e = FunkySingle $ \ st -> return (Left e, st)
    catchError ma f = FunkySingle $ \ st -> do
        (mx, st') <- runFunkySingle st ma
        case mx of
            Left  x -> runFunkySingle st' (f x)
            Right x -> return (Right x, st')

instance Monad m => MonadState st (FunkySingle st err m) where
    {-# INLINE get #-}
    {-# INLINE put #-}
    get = FunkySingle $ \ st -> return (Right st, st)
    put st = FunkySingle $ \ _ -> return (Right (), st)

instance MonadIO m => MonadIO (FunkySingle st err m) where
    {-# INLINE liftIO #-}
    liftIO io = FunkySingle $ \ st -> do
        a <- liftIO io
        return (Right a, st)

instance MonadTrans (FunkySingle st err) where
    {-# INLINE lift #-}
    lift ma = FunkySingle $ \ st -> do
        x <- ma
        return (Right x, st)

instance MonadBase b m => MonadBase b (FunkySingle st err m) where
    liftBase = liftBaseDefault

