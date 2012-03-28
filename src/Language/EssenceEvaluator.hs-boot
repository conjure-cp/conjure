{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.EssenceEvaluator where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer

import GenericOps.Core
import PrintUtils

import {-# SOURCE #-} Language.Essence.Value
import {-# SOURCE #-} Language.Essence.Domain
import {-# SOURCE #-} Language.Essence.Expr
import {-# SOURCE #-} Language.Essence.Spec



--------------------------------------------------------------------------------
-- partial evaluator -----------------------------------------------------------
--------------------------------------------------------------------------------

class Simplify a where
    simplify ::
        ( Applicative m
        , Monad m
        , MonadError Doc m
        , MonadState BindingsMap m
        , MonadWriter [Doc] m
        ) => a -> m (Maybe a)

runSimplify :: (Applicative m, MonadError Doc m, MonadWriter [Doc] m) => Spec -> m Spec

deepSimplify ::
    ( GPlate a
    , Applicative m
    , Monad m
    , MonadError Doc m
    , MonadState BindingsMap m
    , MonadWriter [Doc] m
    ) => a -> m a

simplifyReal ::
    ( Applicative m
    , Monad m
    , MonadError Doc m
    , MonadState BindingsMap m
    , MonadWriter [Doc] m
    ) => Expr -> m (Maybe Expr)



domSize :: Domain -> Maybe Expr


instance Simplify Expr

exprUnify :: Expr -> Expr -> Bool


--------------------------------------------------------------------------------
-- full evaluator --------------------------------------------------------------
--------------------------------------------------------------------------------

class Evaluate a b where
    evaluate :: ( Applicative m
                , Monad m
                , MonadError Doc m
                , MonadState BindingsMap m
                , MonadWriter [Doc] m
                ) => a -> m b

instance (Evaluate a1 a2, Evaluate b1 b2) => Evaluate (a1,b1) (a2,b2)

instance (Evaluate a1 a2, Evaluate b1 b2, Evaluate c1 c2) => Evaluate (a1,b1,c1) (a2,b2,c2)

instance (Evaluate a b) => Evaluate [a] [b]

instance Evaluate Expr Value

instance Evaluate Expr Expr

instance Evaluate Value Value

instance Evaluate Expr Integer

instance Evaluate Value Integer

instance Evaluate Expr Int

instance Evaluate Expr Bool

instance Evaluate Value Bool
