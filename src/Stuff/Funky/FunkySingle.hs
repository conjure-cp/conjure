{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Stuff.Funky.FunkySingle
    ( FunkySingle(..), runFunkySingle
    ) where

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
{-# INLINEABLE runFunkySingle #-}

instance Monad m => Functor (FunkySingle st err m) where
    {-# SPECIALISE instance Functor (FunkySingle st err Identity) #-}
    {-# SPECIALISE instance Functor (FunkySingle st err IO      ) #-}
    {-# INLINEABLE fmap #-}
    fmap f (FunkySingle g) = FunkySingle $ \ st -> do
        (mx, st') <- g st
        return $ case mx of
            Left  x -> (Left  x    , st')
            Right x -> (Right (f x), st')

instance Monad m => Applicative (FunkySingle st err m) where
    {-# SPECIALISE instance Applicative (FunkySingle st err Identity) #-}
    {-# SPECIALISE instance Applicative (FunkySingle st err IO      ) #-}
    {-# INLINEABLE pure #-}
    {-# INLINEABLE (<*>) #-}
    pure x = FunkySingle $ \ st -> return (Right x, st)
    (<*>) = ap

instance Monad m => Monad (FunkySingle st err m) where
    {-# SPECIALISE instance Monad (FunkySingle st err Identity) #-}
    {-# SPECIALISE instance Monad (FunkySingle st err IO      ) #-}
    {-# INLINEABLE fail #-}
    {-# INLINEABLE return #-}
    {-# INLINEABLE (>>=) #-}
    fail = error
    return = pure
    FunkySingle g >>= f = FunkySingle $ \ st -> do
        (mx, st') <- g st
        case mx of
            Left  x -> return (Left x, st')
            Right x -> runFunkySingle st' (f x)

instance Monad m => MonadError err (FunkySingle st err m) where
    {-# SPECIALISE instance MonadError err (FunkySingle st err Identity) #-}
    {-# SPECIALISE instance MonadError err (FunkySingle st err IO      ) #-}
    {-# INLINEABLE throwError #-}
    {-# INLINEABLE catchError #-}
    throwError e = FunkySingle $ \ st -> return (Left e, st)
    catchError ma f = FunkySingle $ \ st -> do
        (mx, st') <- runFunkySingle st ma
        case mx of
            Left  x -> runFunkySingle st' (f x)
            Right x -> return (Right x, st')

instance Monad m => MonadState st (FunkySingle st err m) where
    {-# SPECIALISE instance MonadState st (FunkySingle st err Identity) #-}
    {-# SPECIALISE instance MonadState st (FunkySingle st err IO      ) #-}
    {-# INLINEABLE get #-}
    {-# INLINEABLE put #-}
    get = FunkySingle $ \ st -> return (Right st, st)
    put st = FunkySingle $ \ _ -> return (Right (), st)

instance MonadIO m => MonadIO (FunkySingle st err m) where
    {-# SPECIALISE instance MonadIO (FunkySingle st err IO) #-}
    {-# INLINEABLE liftIO #-}
    liftIO io = FunkySingle $ \ st -> do
        a <- liftIO io
        return (Right a, st)

instance MonadTrans (FunkySingle st err) where
    {-# INLINEABLE lift #-}
    lift ma = FunkySingle $ \ st -> do
        x <- ma
        return (Right x, st)

instance MonadBase b m => MonadBase b (FunkySingle st err m) where
    liftBase = liftBaseDefault

