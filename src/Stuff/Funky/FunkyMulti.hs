{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

module Stuff.Funky.FunkyMulti
    ( FunkyMulti(..), runFunkyMulti
    , fmGetsGlobal, fmModifyGlobal
    , seqListSpine
    ) where

-- conjure
import Conjure.Prelude
import Stuff.MonadList

import Control.Monad.Base        ( MonadBase(..), liftBaseDefault )
import Control.Monad.State       ( MonadState(..) )

-- base
import GHC.Prim ( seq )


data Tree a
    = Nil
    | Single a
    | ListOf [a]
    | Cons a (Tree a)
    | Append (Tree a) (Tree a)
    deriving Functor

{-# INLINEABLE treeToList #-}
treeToList :: Tree a -> [a]
treeToList Nil = []
treeToList (Single x) = [x]
treeToList (ListOf xs) = xs
treeToList (Cons x ys) = x : treeToList ys
treeToList (Append xs ys) = treeToList xs ++ treeToList ys

{-# INLINEABLE firstInTree #-}
firstInTree :: Tree a -> (Maybe a, Tree a)
firstInTree Nil = (Nothing, Nil)
firstInTree (Single x) = (Just x, Nil)
firstInTree (ListOf []) = (Nothing, Nil)
firstInTree (ListOf (x:xs)) = (Just x, ListOf xs)
firstInTree (Cons x t) = (Just x, t)
firstInTree (Append xs ys) =
    case firstInTree xs of
        (Nothing, Nil ) -> firstInTree ys
        (Nothing, _   ) -> error "Invariant violation, firstInTree"
        (Just x , Nil ) -> (Just x, ys)
        (Just x , rest) -> (Just x, Append rest ys)



newtype FunkyMulti g st err m a = FunkyMulti (g -> st -> m (Tree (Either err a, st), g))

runFunkyMulti :: Monad m => g -> st -> FunkyMulti g st err m a -> m ([(Either err a, st)], g)
runFunkyMulti g st ma = liftM (first treeToList) $ runFunkyMultiInternal g st ma

runFunkyMultiInternal :: Monad m => g -> st -> FunkyMulti g st err m a -> m (Tree (Either err a, st), g)
runFunkyMultiInternal g st (FunkyMulti f) = f g st
{-# INLINEABLE runFunkyMultiInternal #-}

fmGetsGlobal :: Monad m => (g -> a) -> FunkyMulti g st err m a
fmGetsGlobal f = FunkyMulti $ \ g st -> let res = f g in res `seq` return (Single (Right res, st), g)
{-# INLINEABLE fmGetsGlobal #-}

fmModifyGlobal :: Monad m => (g -> g) -> FunkyMulti g st err m ()
fmModifyGlobal f = FunkyMulti $ \ g st -> let res = f g in res `seq` return (Single (Right (), st), res)
{-# INLINEABLE fmModifyGlobal #-}

seqListSpine :: [a] -> b -> b
seqListSpine []     b = b
seqListSpine (_:as) b = seqListSpine as b
{-# INLINEABLE seqListSpine #-}

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
            fmap (first (fmap (first g))) (runFunkyMultiInternal glob st m)

instance (Functor m, Monad m) => Applicative (FunkyMulti g st err m) where
    {-# SPECIALISE instance Applicative (FunkyMulti g st err Identity) #-}
    {-# SPECIALISE instance Applicative (FunkyMulti g st err IO      ) #-}
    {-# INLINEABLE pure #-}
    {-# INLINEABLE (<*>) #-}
    pure x = FunkyMulti $ \ g st -> return (Single (Right x, st), g)
    (<*>) = ap

instance (Functor m, Monad m) => Monad (FunkyMulti g st err m) where
    {-# SPECIALISE instance Monad (FunkyMulti g st err Identity) #-}
    {-# SPECIALISE instance Monad (FunkyMulti g st err IO      ) #-}
    {-# INLINEABLE return #-}
    {-# INLINEABLE (>>=) #-}
    return = pure
    FunkyMulti g >>= f = FunkyMulti $ \ glob st -> do
        (results, global') <- g glob st
        doOne results global'
        where
            {-# INLINEABLE doOne #-}
            doOne (firstInTree -> (Nothing, Nil)) gl = return (Nil, gl)
            doOne (firstInTree -> (Just (Left  x, l), rest)) gl = do
                ~(rest', gl') <- doOne rest gl
                return (Cons (Left x, l) rest', gl')
            doOne (firstInTree -> (Just (Right x, l), rest)) gl = do
                ~(rest' , gl' ) <- runFunkyMultiInternal gl l (f x)
                ~(rest'', gl'') <- doOne rest gl'
                return (Append rest' rest'', gl'')
            doOne _ _ = error "Invariant violation, Monad.doOne"

instance (Functor m, Monad m) => MonadError err (FunkyMulti g st err m) where
    {-# SPECIALISE instance MonadError err (FunkyMulti g st err Identity) #-}
    {-# SPECIALISE instance MonadError err (FunkyMulti g st err IO      ) #-}
    {-# INLINEABLE throwError #-}
    {-# INLINEABLE catchError #-}
    throwError e = FunkyMulti $ \ g st -> return (Single (Left e, st), g)
    catchError (FunkyMulti g) f = FunkyMulti $ \ glob st -> do
        (results, global') <- g glob st
        doOne results global'
        where
            {-# INLINEABLE doOne #-}
            doOne (firstInTree -> (Nothing, Nil)) gl = return (Nil, gl)
            doOne (firstInTree -> (Just (Left  x, l), rest)) gl = do
                ~(rest' , gl' ) <- runFunkyMultiInternal gl l (f x)
                ~(rest'', gl'') <- doOne rest gl'
                return (Append rest' rest'', gl'')
            doOne (firstInTree -> (Just (Right x, l), rest)) gl = do
                ~(rest', gl') <- doOne rest gl
                return (Cons (Right x, l) rest', gl')
            doOne _ _ = error "Invariant violation, Monad.doOne"

instance (Functor m, Monad m) => MonadState st (FunkyMulti g st err m) where
    {-# SPECIALISE instance MonadState st (FunkyMulti g st err Identity) #-}
    {-# SPECIALISE instance MonadState st (FunkyMulti g st err IO      ) #-}
    {-# INLINEABLE get #-}
    {-# INLINEABLE put #-}
    get = FunkyMulti $ \ g st -> return (Single (Right st, st), g)
    put st = FunkyMulti $ \ g _ -> return (Single (Right (), st), g)

instance (Functor m, MonadIO m) => MonadIO (FunkyMulti g st err m) where
    {-# SPECIALISE instance MonadIO (FunkyMulti g st err IO) #-}
    {-# INLINEABLE liftIO #-}
    liftIO io = FunkyMulti $ \ g st -> do
        a <- liftIO io
        return (Single (Right a, st), g)

instance MonadTrans (FunkyMulti g st err) where
    {-# INLINEABLE lift #-}
    lift ma = FunkyMulti $ \ g st -> do
        x <- ma
        return (Single (Right x, st), g)

instance MonadBase b m => MonadBase b (FunkyMulti g st err m) where
    liftBase = liftBaseDefault

instance Monad m => MonadList (FunkyMulti g st err m) where
    {-# SPECIALISE instance MonadList (FunkyMulti g st err Identity) #-}
    {-# SPECIALISE instance MonadList (FunkyMulti g st err IO      ) #-}
    returns xs = FunkyMulti $ \ g st -> return (ListOf [ (Right x, st) | x <- xs ], g)

