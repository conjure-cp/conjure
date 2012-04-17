{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Phases.ReadIn ( ReadIn(..), runReadIn ) where

import Control.Applicative
import Control.Monad.Error ( MonadError, throwError, runErrorT )
import Control.Monad.Writer ( MonadWriter, runWriter )
import qualified Data.Text.Lazy as T

import ParsePrint ( ParsePrint, parse )
import PrintUtils ( Doc, text, vcat, ($$) )
import Utils

import Language.Essence
import Language.Essence.Phases.PostParse ( postParse )
import Language.EssenceLexer ( runLexer )
import Language.EssenceLexerP ( Pos(..), runParser, treeToDoc )



class ReadIn a where
    readIn ::
        ( Applicative m
        , MonadError Doc m
        , MonadWriter [Doc] m
        ) => FilePath -> T.Text -> m a

runReadIn :: ReadIn a => FilePath -> T.Text -> (Either Doc a, [Doc])
runReadIn fp contents = runWriter $ runErrorT $ readIn fp contents

runParser' :: (ParsePrint a, Applicative m, MonadError Doc m) => FilePath -> T.Text -> m a
runParser' fp t = do
    ls <- runLexer t
    case runParser parse (Pos (Just fp) 1 1) ls of
        Left tree -> throwError (treeToDoc tree)
        Right [(result, _, [])] -> return result
        Right rs -> throwError $ "Unknown error." $$ vcat (map (text . ppShow) rs)

instance ReadIn Spec where
    readIn fp contents = do
        raw   <- runParser' fp contents
        fixed <- postParse raw
        typeCheckSpec fixed
        return fixed

instance ReadIn RuleRefn where
    readIn fp contents = do
        raw <- runParser' fp contents
        let fixed = raw { refnFilename = fp }
        return fixed

instance ReadIn RuleRepr where
    readIn fp contents = do
        raw <- runParser' fp contents
        let fixed = raw { reprFilename = fp }
        return fixed
