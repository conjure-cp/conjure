{-# LANGUAGE FlexibleContexts #-}

module Language.Essence.Phases.ReadIn where

import Control.Applicative
import Control.Monad.Error ( MonadError )
import Control.Monad.Writer ( MonadWriter )
import Control.Monad.IO.Class ( MonadIO, liftIO )

import ParsecUtils ( parseFromFile )
import ParsePrint ( parse )
import PrintUtils ( Doc )

import Language.Essence.Spec
import Language.Essence.RuleRefn
import Language.Essence.RuleRepr
import Language.Essence.Phases.PostParse ( postParse )


readInSpec ::
    ( Applicative m
    , MonadError Doc m
    , MonadWriter [Doc] m
    , MonadIO m
    ) => FilePath -> m Spec
readInSpec fp = do
    raw   <- liftIO $ parseFromFile parse id fp id
    fixed <- postParse raw
    typeCheckSpec fixed
    return fixed


readInRefn ::
    ( Applicative m
    , MonadError Doc m
    , MonadIO m
    ) => FilePath -> m RuleRefn
readInRefn fp = do
    raw <- liftIO $ parseFromFile parse id fp (\ i -> i {refnFilename = fp} )
    return raw


readInRepr ::
    ( Applicative m
    , MonadError Doc m
    , MonadIO m
    ) => FilePath -> m RuleRepr
readInRepr fp = do
    raw <- liftIO $ parseFromFile parse id fp (\ i -> i {reprFilename = fp} )
    return raw
