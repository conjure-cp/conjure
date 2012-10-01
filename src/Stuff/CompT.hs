{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Stuff.CompT where

import Stuff.MonadList

import Control.Applicative         ( Applicative(..) )
import Control.Monad               ( ap, forM )
import Control.Monad.Error         ( MonadError(..), Error(..) )
import Control.Monad.Identity      ( Identity(..) )
import Control.Monad.IO.Class      ( MonadIO, liftIO )
import Control.Monad.Reader        ( MonadReader(..) )
import Control.Monad.State         ( MonadState(..) )
import Control.Monad.Trans.Class   ( MonadTrans, lift )
import Control.Monad.Writer        ( MonadWriter(..) )
import Control.Parallel.Strategies ( parMap, rseq )
import Data.Monoid                 ( Monoid, mempty, mappend )
import Text.PrettyPrint            ( Doc, text )


instance Error Doc where strMsg = text

newtype CompT r s w e m a = CompT ( r -> s -> m [(Either e a, s, w)] )

runComp :: r -> s -> CompT r s w e Identity a -> [(Either e a, s, w)]
runComp r s comp = runIdentity $ runCompT r s comp

runCompT :: Monad m => r -> s -> CompT r s w e m a -> m [(Either e a, s, w)]
runCompT r s (CompT f) = f r s

instance (Monad m, Monoid w) => MonadList (CompT r s w e m) where
    returns xs = CompT $ \ _ s -> return [ (Right x, s, mempty) | x <- xs ]

instance Monad m => Functor (CompT r s w e m) where
    fmap f (CompT g) = CompT $ \ r s -> do
        results <- g r s
        forM results $ \ one -> return $ case one of
            (Left  e, s', w) -> (Left  e    , s', w)
            (Right x, s', w) -> (Right (f x), s', w)

instance (Monad m, Monoid w) => Applicative (CompT r s w e m) where
    pure x = CompT $ \ _ s -> return [(Right x, s, mempty)]
    (<*>) = ap

instance (Monad m, Monoid w) => Monad (CompT r s w e m) where
    return = pure
    CompT cx >>= f = CompT $ \ r s -> do
        xs  <- cx r s
        zss <- forM xs $ \ x -> case x of
            (Left  e, s', w) -> return [(Left e, s', w)]
            (Right a, s', w) -> do
                ys <- runCompT r s' (f a)
                return $ parMap rseq (\ (b,s'',w') -> (b,s'',w `mappend` w') ) ys
        return (concat zss)

instance (Monad m, Monoid w) => MonadError e (CompT r s w e m) where
    throwError e = CompT $ \ _ s -> return [(Left e, s, mempty)]
    catchError ma f = CompT $ \ r s -> do
        xs  <- runCompT r s ma
        zss <- forM xs $ \ x -> case x of
            (Left  e, s', w) -> do
                ys <- runCompT r s' (f e)
                return $ map (\ (a,s'',w') -> (a,s'',w `mappend` w') ) ys
            (Right a, s', w) -> return [(Right a, s', w)]
        return (concat zss)

instance (Monad m, Monoid w) => MonadReader r (CompT r s w e m) where
    ask = CompT $ \ r s -> return [(Right r, s, mempty)]
    local f ma = CompT $ \ r s -> runCompT (f r) s ma

instance (Monad m, Monoid w) => MonadWriter w (CompT r s w e m) where
    tell w = CompT $ \ _ s -> return [(Right (), s, w)]
    listen ma = CompT $ \ r s -> do
        xs <- runCompT r s ma
        return ( flip map xs $ \ x -> case x of
                                    (Left  e, s', w) -> (Left  e    , s', w)
                                    (Right a, s', w) -> (Right (a,w), s', w)
               )
    pass maf = CompT $ \ r s -> do
        xs <- runCompT r s maf
        return ( flip map xs $ \ x -> case x of
                                    (Left  e    , s', w) -> (Left  e, s', w  )
                                    (Right (a,f), s', w) -> (Right a, s', f w)
               )

instance (Monad m, Monoid w) => MonadState s (CompT r s w e m) where
    get   = CompT $ \ _ s -> return [(Right s , s, mempty)]
    put s = CompT $ \ _ _ -> return [(Right (), s, mempty)]

instance (MonadIO m, Monoid w) => MonadIO (CompT r s w e m) where
    liftIO io = CompT $ \ _ s -> do
        a <- liftIO io
        return [(Right a, s, mempty)]

instance Monoid w => MonadTrans (CompT r s w e) where
    lift ma = CompT $ \ _ s -> do
        x <- ma
        return [(Right x, s, mempty)]
