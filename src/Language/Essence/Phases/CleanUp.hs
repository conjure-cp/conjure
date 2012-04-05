{-# LANGUAGE FlexibleContexts #-}

module Language.Essence.Phases.CleanUp where

import Prelude hiding ( mapM )
import Control.Applicative
import Control.Monad.Error ( MonadError )
import Control.Monad.State ( MonadState, get, put, evalStateT )
import Data.List ( nub )
import Data.Traversable ( mapM )
import qualified Data.Map as M

import Constants ( FreshName, mkPrettyFreshNames, getFreshName )
import GenericOps.Core ( universe, bottomUp )
import PrintUtils ( Doc )

import Language.Essence



cleanUp :: (Applicative m, MonadError Doc m) => Spec -> m Spec
cleanUp spec = quanRenameFinal (topLevelConstraints spec)


topLevelConstraints :: Spec -> Spec
topLevelConstraints spec = spec { constraints = concatMap conjunct' $ constraints spec }
    where
        conjunct' :: Expr -> [Expr]
        conjunct' (EOp And [x,y]) = conjunct' x ++ conjunct' y
        conjunct' x = [x]


quanRenameFinal ::
    ( Applicative m
    , MonadError Doc m
    ) => Spec -> m Spec
quanRenameFinal spec = do
    let qNames = mkPrettyFreshNames $ nub [ nm | Identifier nm <- universe spec ]
    flip evalStateT qNames $ do
        topLevels'   <- mapM (withState . workerTL  ) (topLevels   spec)
        constraints' <- mapM (withState . workerExpr) (constraints spec)
        objective'   <- mapM (withState . workerObj ) (objective   spec)
        return $ spec { topLevels  = topLevels'
                      , objective  = objective'
                      , constraints = constraints'
                      }

    where
        workerExpr :: (Applicative m, MonadError Doc m, MonadState [FreshName] m) => Expr -> m Expr
        workerExpr (Q       x) = Q <$> workerQ x
        workerExpr (Bubble x y zs) = Bubble <$> (withState $ workerExpr x)
                                            <*> (withState $ workerExpr y)
                                            <*> (mapM (withState . workerTL) zs)
        workerExpr (EOp op xs) = EOp op <$> mapM (withState . workerExpr) xs
        workerExpr x           = return x

        workerObj :: (Applicative m, MonadError Doc m, MonadState [FreshName] m) => Objective -> m Objective
        workerObj (OMin x) = OMin <$> workerExpr x
        workerObj (OMax x) = OMax <$> workerExpr x

        workerTL :: (Applicative m, MonadError Doc m, MonadState [FreshName] m) => Either Binding Where -> m (Either Binding Where)
        workerTL (Left (LettingExpr i x)) = Left . LettingExpr i <$> (withState . workerExpr) x
        workerTL (Left  b)                = Left                 <$> return b
        workerTL (Right (Where w))        = Right . Where        <$> (withState . workerExpr) w

        workerQ :: (Applicative m, MonadError Doc m, MonadState [FreshName] m) => QuantifiedExpr -> m QuantifiedExpr
        workerQ p@(QuantifiedExpr {quanVar = Left (Identifier old)}) = do
            new <- getFreshName
            return $ bottomUp (identifierRenamer old new) p
        workerQ p@(QuantifiedExpr {quanVar = Right qnSVar}) = do
            let
                -- rec :: StructuredVar -> m (M.Map String String)
                rec (I (Identifier old)) = do
                    new <- getFreshName
                    return $ M.singleton old new
                rec (STuple  ps) = M.unions <$> mapM rec ps
                rec (SMatrix ps) = M.unions <$> mapM rec ps

            lu <- rec qnSVar

            let f i@(Identifier nm) = case M.lookup nm lu of
                                            Nothing  -> i
                                            Just new -> Identifier new

            return $ bottomUp f p


withState :: MonadState s m => m a -> m a
withState comp = do
    st <- get
    res <- comp
    put st
    return res
