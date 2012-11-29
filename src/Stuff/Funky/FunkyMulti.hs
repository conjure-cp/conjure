{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Stuff.Funky.FunkyMulti
    ( FunkyMulti(..), runFunkyMulti
    , fmGetsGlobal, fmModifyGlobal
    ) where

import Control.Applicative       ( Applicative(..) )
import Control.Arrow             ( first )
import Control.Monad             ( ap )
import Control.Monad.Base        ( MonadBase(..), liftBaseDefault )
import Control.Monad.Error       ( MonadError(..) )
import Control.Monad.Identity    ( Identity )
import Control.Monad.IO.Class    ( MonadIO, liftIO )
import Control.Monad.State       ( MonadState(..) )
import Control.Monad.Trans.Class ( MonadTrans, lift )

import Stuff.MonadList


newtype FunkyMulti g st err m a = FunkyMulti (g -> st -> m ([(Either err a, st)], g))

runFunkyMulti :: Monad m => g -> st -> FunkyMulti g st err m a -> m ([(Either err a, st)], g)
runFunkyMulti g st (FunkyMulti f) = f g st
{-# INLINEABLE runFunkyMulti #-}

fmGetsGlobal :: Monad m => (g -> a) -> FunkyMulti g st err m a
fmGetsGlobal f = FunkyMulti $ \ g st -> return ([(Right (f g), st)], g)
{-# INLINEABLE fmGetsGlobal #-}

fmModifyGlobal :: Monad m => (g -> g) -> FunkyMulti g st err m ()
fmModifyGlobal f = FunkyMulti $ \ g st -> return ([(Right (), st)], f g)
{-# INLINEABLE fmModifyGlobal #-}

instance (Functor m, Monad m) => Functor (FunkyMulti g st err m) where
    {-# SPECIALISE instance Functor (FunkyMulti g st err Identity) #-}
    {-# SPECIALISE instance Functor (FunkyMulti g st err IO      ) #-}
    {-# INLINEABLE fmap #-}
    fmap f m = FunkyMulti $ \ glob st ->
        let
            {-# INLINEABLE g #-}
            g (Left  x) = Left x
            g (Right x) = Right (f x)
        in
            fmap (first (map (first g))) (runFunkyMulti glob st m)

instance (Functor m, Monad m) => Applicative (FunkyMulti g st err m) where
    {-# SPECIALISE instance Applicative (FunkyMulti g st err Identity) #-}
    {-# SPECIALISE instance Applicative (FunkyMulti g st err IO      ) #-}
    {-# INLINEABLE pure #-}
    {-# INLINEABLE (<*>) #-}
    pure x = FunkyMulti $ \ g st -> return ([(Right x, st)], g)
    (<*>) = ap

instance (Functor m, Monad m) => Monad (FunkyMulti g st err m) where
    {-# SPECIALISE instance Monad (FunkyMulti g st err Identity) #-}
    {-# SPECIALISE instance Monad (FunkyMulti g st err IO      ) #-}
    {-# INLINEABLE fail #-}
    {-# INLINEABLE return #-}
    {-# INLINEABLE (>>=) #-}
    fail = error
    return = pure
    FunkyMulti g >>= f = FunkyMulti $ \ glob st -> do
        (results, global') <- g glob st
        doOne results global'
        where
            {-# INLINEABLE doOne #-}
            doOne [] gl = return ([], gl)
            doOne ((Left  e, l) : rest) gl = do
                (rest', gl') <- doOne rest gl
                return ((Left e, l) : rest', gl')
            doOne ((Right x, l) : rest) gl = do
                (rest' , gl' ) <- runFunkyMulti gl l (f x)
                (rest'', gl'') <- doOne rest gl'
                return (rest' ++ rest'', gl'')

instance (Functor m, Monad m) => MonadError err (FunkyMulti g st err m) where
    {-# SPECIALISE instance MonadError err (FunkyMulti g st err Identity) #-}
    {-# SPECIALISE instance MonadError err (FunkyMulti g st err IO      ) #-}
    {-# INLINEABLE throwError #-}
    {-# INLINEABLE catchError #-}
    throwError e = FunkyMulti $ \ g st -> return ([(Left e, st)], g)
    catchError (FunkyMulti g) f = FunkyMulti $ \ glob st -> do
        (results, global') <- g glob st
        doOne results global'
        where
            {-# INLINEABLE doOne #-}
            doOne [] gl = return ([], gl)
            doOne ((Left  x, l) : rest) gl = do
                (rest' , gl' ) <- runFunkyMulti gl l (f x)
                (rest'', gl'') <- doOne rest gl'
                return (rest' ++ rest'', gl'')
            doOne ((Right x, l) : rest) gl = do
                (rest', gl') <- doOne rest gl
                return ((Right x, l) : rest', gl')

instance (Functor m, Monad m) => MonadState st (FunkyMulti g st err m) where
    {-# SPECIALISE instance MonadState st (FunkyMulti g st err Identity) #-}
    {-# SPECIALISE instance MonadState st (FunkyMulti g st err IO      ) #-}
    {-# INLINEABLE get #-}
    {-# INLINEABLE put #-}
    get = FunkyMulti $ \ g st -> return ([(Right st, st)], g)
    put st = FunkyMulti $ \ g _ -> return ([(Right (), st)], g)

instance (Functor m, MonadIO m) => MonadIO (FunkyMulti g st err m) where
    {-# SPECIALISE instance MonadIO (FunkyMulti g st err IO) #-}
    {-# INLINEABLE liftIO #-}
    liftIO io = FunkyMulti $ \ g st -> do
        a <- liftIO io
        return ([(Right a, st)], g)

instance MonadTrans (FunkyMulti g st err) where
    {-# INLINEABLE lift #-}
    lift ma = FunkyMulti $ \ g st -> do
        x <- ma
        return ([(Right x, st)], g)

instance MonadBase b m => MonadBase b (FunkyMulti g st err m) where
    liftBase = liftBaseDefault

instance Monad m => MonadList (FunkyMulti g st err m) where
    {-# SPECIALISE instance MonadList (FunkyMulti g st err Identity) #-}
    {-# SPECIALISE instance MonadList (FunkyMulti g st err IO      ) #-}
    returns xs = FunkyMulti $ \ g st -> return ([ (Right x, st) | x <- xs ], g)

