{-# LANGUAGE FlexibleContexts #-}

module Phases.ResolveTwoBars ( resolveTwoBars ) where

import Prelude hiding ( mapM )
import Data.Generics.Uniplate.Direct
import Control.Monad ( liftM )
import Data.Traversable ( mapM )
import Data.Default ( def )
import Control.Monad.Error ( MonadError, runErrorT )
import Control.Monad.RWS ( MonadReader, MonadWriter, MonadState, evalRWS )

import Language.Essence
import Language.EssenceTypes ( typeOf )


resolveTwoBars :: Spec -> (Either String Spec, [Log])
resolveTwoBars sp = evalRWS (runErrorT (core sp)) (topLevelBindings sp) def


-- for the type of core, see Language.EssenceTypes.typeOf
core ::
    ( Biplate Spec Expr
    , MonadWriter [Log] m
    , MonadReader [Binding] m
    , MonadError String m
    , MonadState ([String],[(String,Type)]) m
    ) => Spec -> m Spec
core = transformBiM' f -- needs to be top-down, since state is passed from top to down.
    where
        f (GenericNode Abs [x]) = do
            let returnAbs  = return $ GenericNode Abs [x]
            let returnCard = return $ GenericNode Card [x]
            tx <- typeOf x
            case tx of
                TypeSet  {} -> returnCard
                TypeMSet {} -> returnCard
                _           -> returnAbs
        f x = return x


-- top-down version of transformM
transformM' :: (Monad m, Uniplate on) => (on -> m on) -> on -> m on
transformM' f x = descendM (transformM' f) =<< f x

-- top-down version of transformBiM
transformBiM' :: (Monad m, Biplate from to) => (to -> m to) -> from -> m from
transformBiM' f x = liftM generate $ mapM (transformM' f) current
    where (current, generate) = biplate x
