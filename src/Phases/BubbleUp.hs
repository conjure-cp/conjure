{-# LANGUAGE FlexibleContexts #-}

module Phases.BubbleUp ( bubbleUp ) where

import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.State ( MonadState, runStateT )
import Data.Generics.Uniplate.Direct ( descendM )

import Has
import Language.Essence


bubbleUp :: MonadIO m => Spec -> m Spec
bubbleUp sp = do
    (objective', toPosts1, binds1)
        <- case objective sp of
            Nothing -> return (Nothing, [], [])
            Just (oEnum,x) -> do
                (x',(p,b)) <- runStateT (bubbleUpConstraint x) ([],[])
                return $ (Just (oEnum,x'),p,b)
    (constraints',(toPosts2,binds2)) <- runStateT (mapM bubbleUpConstraint (constraints sp)) ([],[])

    return sp { topLevelBindings = concat $ topLevelBindings sp : binds1   ++ binds2
              , constraints      = concat $ constraints'        : toPosts1 ++ toPosts2
              , objective        = objective'
              }

bubbleUpConstraint ::
    ( MonadIO m
    , MonadState st m
    , Has st [[Expr]]
    , Has st [[Binding]]
    ) => Expr -> m Expr
bubbleUpConstraint (Bubble actual toPost binds) = do
    modifyM (toPost:)
    modifyM (binds :)
    bubbleUpConstraint actual
bubbleUpConstraint x = descendM (bubbleUpConstraint) x
