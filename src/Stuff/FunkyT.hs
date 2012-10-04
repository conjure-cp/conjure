{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Stuff.FunkyT where

import Stuff.MonadList

import Control.Applicative         ( Applicative(..) )
import Control.Monad               ( ap, forM )
import Control.Monad.Error         ( MonadError(..) )
import Control.Monad.Identity      ( Identity(..) )
import Control.Monad.IO.Class      ( MonadIO, liftIO )
import Control.Monad.Trans.Class   ( MonadTrans, lift )


newtype FunkyT localSt globalSt err m a = FunkyT ( localSt -> globalSt -> m ([(Either err a, localSt)], globalSt) )

runFunkyT :: localSt -> globalSt -> FunkyT localSt globalSt err m a -> m ([(Either err a, localSt)], globalSt)
runFunkyT local global (FunkyT f) = f local global

runFunky :: localSt -> globalSt -> FunkyT localSt globalSt err Identity a -> ([(Either err a, localSt)], globalSt)
runFunky local global ma = runIdentity $ runFunkyT local global ma

instance Monad m => MonadList (FunkyT localSt globalSt err m) where
    {-# INLINE returns #-}
    returns xs = FunkyT $ \ local global -> return ([ (Right x, local) | x <- xs ], global)

instance Monad m => Functor (FunkyT localSt globalSt err m) where
    {-# INLINE fmap #-}
    fmap f (FunkyT g) = FunkyT $ \ local global -> do
        (results, global') <- g local global
        results' <- forM results $ \ one -> return $ case one of
            (Left  e, local') -> (Left  e    , local')
            (Right x, local') -> (Right (f x), local')
        return (results', global')

instance Monad m => Applicative (FunkyT localSt globalSt err m) where
    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}
    pure x = FunkyT $ \ local global -> let result = ([(Right x, local)], global) in return result
    (<*>) = ap

instance Monad m => Monad (FunkyT localSt globalSt err m) where
    {-# INLINE fail #-}
    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    fail = error
    return = pure
    FunkyT g >>= f = FunkyT $ \ local global -> do
        (results, global') <- g local global
        let
            {-# INLINE doOne #-}
            -- doOne :: Monad m => (a -> FunkyT localSt globalSt err m b) -> [(Either err a, localSt)] -> globalSt -> m ([(Either err b, localSt)], globalSt)
            doOne [] gl = return ([], gl)
            doOne ((Left  e, l) : rest) gl = do
                (rest', gl') <- doOne rest gl
                return ((Left e, l) : rest', gl')
            doOne ((Right x, l) : rest) gl = do
                (rest' , gl' ) <- runFunkyT l gl (f x)
                (rest'', gl'') <- doOne rest gl'
                let rests = rest' ++ rest''
                return (rests, gl'')
        doOne results global'

instance Monad m => MonadError err (FunkyT localSt globalSt err m) where
    {-# INLINE throwError #-}
    {-# INLINE catchError #-}
    throwError e = FunkyT $ \ local global -> return ([(Left e, local)], global)
    catchError ma f = FunkyT $ \ local global -> do
        (results, global') <- runFunkyT local global ma
        let
            doOne [] g = return ([], g)
            doOne ((Left  e, l) : rest) g = do
                (rest' , g' ) <- runFunkyT l g (f e)
                (rest'', g'') <- doOne rest g'
                return (rest' ++ rest'', g'')
            doOne ((Right x, l) : rest) g = do
                (rest', g') <- doOne rest g
                return ((Right x, l) : rest', g')
        doOne results global'

{-# INLINE getsLocal #-}
{-# INLINE getsGlobal #-}
{-# INLINE modifyLocal #-}
{-# INLINE modifyGlobal #-}

getsLocal    :: Monad m => (localSt  -> a) -> FunkyT localSt globalSt err m a
getsGlobal   :: Monad m => (globalSt -> a) -> FunkyT localSt globalSt err m a
modifyLocal  :: Monad m => (localSt  -> localSt ) -> FunkyT localSt globalSt err m ()
modifyGlobal :: Monad m => (globalSt -> globalSt) -> FunkyT localSt globalSt err m ()

getsLocal    f = FunkyT $ \ local global -> return ([(Right (f local ), local)], global)
getsGlobal   f = FunkyT $ \ local global -> return ([(Right (f global), local)], global)
modifyLocal  f = FunkyT $ \ local global -> return ([(Right (), f local)],   global)
modifyGlobal f = FunkyT $ \ local global -> return ([(Right (),   local)], f global)

instance MonadIO m => MonadIO (FunkyT localSt globalSt err m) where
    {-# INLINE liftIO #-}
    liftIO io = FunkyT $ \ local global -> do
        a <- liftIO io
        return ([(Right a, local)], global)

instance MonadTrans (FunkyT localSt globalSt err) where
    {-# INLINE lift #-}
    lift ma = FunkyT $ \ local global -> do
        x <- ma
        return ([(Right x, local)], global)

