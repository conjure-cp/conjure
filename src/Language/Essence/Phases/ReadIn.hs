{-# LANGUAGE FlexibleContexts #-}

module Language.Essence.Phases.ReadIn ( ReadIn(..), runReadIn ) where

import Control.Applicative
import Control.Monad.Error ( MonadError, runErrorT )
import Control.Monad.Writer ( MonadWriter, runWriter )

import ParsePrint ( runParser )
import PrintUtils ( Doc )

import Language.Essence.Spec
import Language.Essence.RuleRefn
import Language.Essence.RuleRepr
import Language.Essence.Phases.PostParse ( postParse )



class ReadIn a where
    readIn ::
        ( Applicative m
        , MonadError Doc m
        , MonadWriter [Doc] m
        ) => FilePath -> String -> m a

runReadIn :: ReadIn a => FilePath -> String -> (Either Doc a, [Doc])
runReadIn fp contents = runWriter $ runErrorT $ readIn fp contents

instance ReadIn Spec where
    readIn fp contents = do
        raw   <- runParser fp contents
        fixed <- postParse raw
        typeCheckSpec fixed
        return fixed

instance ReadIn RuleRefn where
    readIn fp contents = do
        raw <- runParser fp contents
        let fixed = raw { refnFilename = fp }
        return fixed

instance ReadIn RuleRepr where
    readIn fp contents = do
        raw <- runParser fp contents
        let fixed = raw { reprFilename = fp }
        return fixed
