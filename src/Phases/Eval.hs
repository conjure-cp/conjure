{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Phases.Eval where

import Control.Applicative
import Control.Monad.IO.Class ( MonadIO )
import Data.Generics.Uniplate.Direct ( descendM, descendBiM )
import Control.Monad.State ( MonadState, evalStateT )

import Language.Essence
import Language.EssenceEvaluator ( runEvaluateExpr, quanDomain )
import Has


evalSpec :: (Applicative m, MonadIO m) => Spec -> m Spec
evalSpec s = evalStateT (descendBiM evaluatorFunc s) (topLevelBindings s)

evaluatorFunc ::
    ( Applicative m
    , MonadIO m
    , MonadState st m
    , Has st [Binding]
    ) => Expr -> m Expr
evaluatorFunc x@(ExprQuantifier{quanVar=Identifier nm, quanOver}) = do
    binds <- getM
    putM $ (Quantified,nm,quanDomain quanOver) : binds
    x'          <- descendM evaluatorFunc x
    (x'',_logs) <- runEvaluateExpr binds x'
    putM binds
    return x''
evaluatorFunc x = do
    binds <- getM
    x' <- descendM evaluatorFunc x
    (x'', _logs) <- runEvaluateExpr binds x'
    return x''
