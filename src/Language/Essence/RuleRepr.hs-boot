{-# LANGUAGE FlexibleContexts #-}

module Language.Essence.RuleRepr where

import Control.Applicative ( Applicative )
import Control.Monad.Error ( MonadError )
import Control.Monad.State ( MonadState )
import Control.Monad.Writer ( MonadWriter )

import GenericOps.Core ( BindingsMap )
import PrintUtils ( Doc )

import Language.Essence.Domain
import Language.Essence.Where
import Language.Essence.Binding
import Language.Essence.Expr



data RuleRepr = RuleRepr
    { reprFilename   :: String
    , reprName       :: String
    , reprTemplate   :: Domain
    , reprStructural :: Maybe Expr
    , reprLocals     :: [Either Binding Where]
    , reprCases      :: [RuleReprCase]
    }

data RuleReprCase = RuleReprCase
    { reprCasePattern    :: Domain
    , reprCaseStructural :: Maybe Expr
    , reprCaseLocals     :: [Either Binding Where]
    }

type ReprResult = ( String     -- name of the representation
                  , Domain     -- replacement domain
                  , [Expr]     -- structural constraints
                  )

applyReprsToDom ::
    ( Applicative m
    , Monad m
    , MonadError Doc m
    , MonadState (BindingsMap, [String]) m
    , MonadWriter [Doc] m
    ) => [RuleRepr]
      -> Domain
      -> m [ReprResult]
