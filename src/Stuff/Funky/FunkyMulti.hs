{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Stuff.Funky.FunkyMulti where

import Control.Applicative       ( Applicative(..) )
import Control.Arrow             ( first )
import Control.Monad             ( ap, forM, liftM )
import Control.Monad.Base        ( MonadBase(..), liftBaseDefault )
import Control.Monad.Error       ( MonadError(..) )
import Control.Monad.Identity    ( Identity )
import Control.Monad.IO.Class    ( MonadIO, liftIO )
import Control.Monad.State       ( MonadState(..) )
import Control.Monad.Trans.Class ( MonadTrans, lift )

import Stuff.MonadList


newtype FunkyMulti st err m a = FunkyMulti (st -> m [(Either err a, st)])

runFunkyMulti :: Monad m => st -> FunkyMulti st err m a -> m [(Either err a, st)]
runFunkyMulti st (FunkyMulti f) = f st

instance (Functor m, Monad m) => Functor (FunkyMulti st err m) where
    {-# SPECIALIZE instance Functor (FunkyMulti st err Identity) #-}
    {-# INLINE fmap #-}
    fmap f m = {-# SCC "FMfmap" #-} FunkyMulti $ \ st ->
        let
            g (Left  x) = Left x
            g (Right x) = Right (f x)
        in
            fmap (map (first g)) (runFunkyMulti st m)

instance (Functor m, Monad m) => Applicative (FunkyMulti st err m) where
    {-# SPECIALIZE instance Applicative (FunkyMulti st err Identity) #-}
    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}
    pure x = FunkyMulti $ \ st -> return [(Right x, st)]
    (<*>) = ap

instance (Functor m, Monad m) => Monad (FunkyMulti st err m) where
    {-# SPECIALIZE instance Monad (FunkyMulti st err Identity) #-}
    {-# INLINE fail #-}
    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    fail = error
    return = pure
    FunkyMulti g >>= f = {-# SCC "FMbind" #-} FunkyMulti $ \ st -> do
        let one (Left  x, l) = return [(Left x, l)]
            one (Right x, l) = runFunkyMulti l (f x)
        results <- g st
        let go (x:xs) = do y <- one x ; ys <- go xs ; return (y:ys)
            go []     = return []
        liftM concat (go results)
        -- liftM concat (mapM one results)

instance (Functor m, Monad m) => MonadError err (FunkyMulti st err m) where
    {-# INLINE throwError #-}
    {-# INLINE catchError #-}
    throwError e = FunkyMulti $ \ st -> return [(Left e, st)]
    catchError ma f = FunkyMulti $ \ st -> do
        results <- runFunkyMulti st ma
        liftM concat $ forM results $ \ ~(mx, l) ->
            case mx of
                Left  x -> runFunkyMulti l (f x)
                Right x -> return [(Right x, l)]
        -- let
            -- doOne [] = return []
            -- doOne ((Left  e, l) : rest) = do
                -- rest'  <- runFunkyMulti l (f e)
                -- rest'' <- doOne rest
                -- return $ rest' ++ rest''
            -- doOne ((Right x, l) : rest) = do
                -- rest' <- doOne rest
                -- return $ (Right x, l) : rest'
        -- doOne results

instance (Functor m, Monad m) => MonadState st (FunkyMulti st err m) where
    {-# INLINE get #-}
    {-# INLINE put #-}
    get = FunkyMulti $ \ st -> return [(Right st, st)]
    put st = FunkyMulti $ \ _ -> return [(Right (), st)]

instance (Functor m, MonadIO m) => MonadIO (FunkyMulti st err m) where
    {-# INLINE liftIO #-}
    liftIO io = FunkyMulti $ \ st -> do
        a <- liftIO io
        return [(Right a, st)]

instance MonadTrans (FunkyMulti st err) where
    {-# INLINE lift #-}
    lift ma = FunkyMulti $ \ st -> do
        x <- ma
        return [(Right x, st)]

instance MonadBase b m => MonadBase b (FunkyMulti st err m) where
    liftBase = liftBaseDefault

instance Monad m => MonadList (FunkyMulti st err m) where
    returns xs = FunkyMulti $ \ st -> return [ (Right x, st) | x <- xs ]

