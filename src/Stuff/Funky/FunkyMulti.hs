{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Stuff.Funky.FunkyMulti where

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

instance (Functor m, Monad m) => Functor (FunkyMulti g st err m) where
    {-# SPECIALIZE instance Functor (FunkyMulti g st err Identity) #-}
    {-# INLINE fmap #-}
    fmap f m = {-# SCC "FMfmap" #-} FunkyMulti $ \ glob st ->
        let
            g (Left  x) = Left x
            g (Right x) = Right (f x)
        in
            fmap (first (map (first g))) (runFunkyMulti glob st m)

instance (Functor m, Monad m) => Applicative (FunkyMulti g st err m) where
    {-# SPECIALIZE instance Applicative (FunkyMulti g st err Identity) #-}
    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}
    pure x = FunkyMulti $ \ g st -> return ([(Right x, st)], g)
    (<*>) = ap

instance (Functor m, Monad m) => Monad (FunkyMulti g st err m) where
    {-# SPECIALIZE instance Monad (FunkyMulti g st err Identity) #-}
    {-# INLINE fail #-}
    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    fail = error
    return = pure
    FunkyMulti g >>= f = {-# SCC "FMbind" #-} FunkyMulti $ \ glob st -> do
        (results, global') <- g glob st
        let
            {-# INLINE doOne #-}
            doOne [] gl = return ([], gl)
            doOne ((Left  e, l) : rest) gl = do
                (rest', gl') <- doOne rest gl
                return ((Left e, l) : rest', gl')
            doOne ((Right x, l) : rest) gl = do
                (rest' , gl' ) <- runFunkyMulti gl l (f x)
                (rest'', gl'') <- doOne rest gl'
                return (rest' ++ rest'', gl'')
        doOne results global'

instance (Functor m, Monad m) => MonadError err (FunkyMulti g st err m) where
    {-# INLINE throwError #-}
    {-# INLINE catchError #-}
    throwError e = FunkyMulti $ \ g st -> return ([(Left e, st)], g)
    catchError (FunkyMulti g) f = {-# SCC "FMcatch" #-} FunkyMulti $ \ glob st -> do
        (results, global') <- g glob st
        let
            {-# INLINE doOne #-}
            doOne [] gl = return ([], gl)
            doOne ((Left  x, l) : rest) gl = do
                (rest' , gl' ) <- runFunkyMulti gl l (f x)
                (rest'', gl'') <- doOne rest gl'
                return (rest' ++ rest'', gl'')
            doOne ((Right x, l) : rest) gl = do
                (rest', gl') <- doOne rest gl
                return ((Right x, l) : rest', gl')
        doOne results global'

instance (Functor m, Monad m) => MonadState st (FunkyMulti g st err m) where
    {-# INLINE get #-}
    {-# INLINE put #-}
    get = FunkyMulti $ \ g st -> return ([(Right st, st)], g)
    put st = FunkyMulti $ \ g _ -> return ([(Right (), st)], g)

instance (Functor m, MonadIO m) => MonadIO (FunkyMulti g st err m) where
    {-# INLINE liftIO #-}
    liftIO io = FunkyMulti $ \ g st -> do
        a <- liftIO io
        return ([(Right a, st)], g)

instance MonadTrans (FunkyMulti g st err) where
    {-# INLINE lift #-}
    lift ma = FunkyMulti $ \ g st -> do
        x <- ma
        return ([(Right x, st)], g)

instance MonadBase b m => MonadBase b (FunkyMulti g st err m) where
    liftBase = liftBaseDefault

instance Monad m => MonadList (FunkyMulti g st err m) where
    returns xs = FunkyMulti $ \ g st -> return ([ (Right x, st) | x <- xs ], g)

