{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Phases.ReadIn ( ReadIn(..), runReadIn ) where

import Control.Applicative
import Control.Monad.Error ( MonadError, runErrorT )
import Control.Monad.Writer ( MonadWriter, runWriter )
import qualified Data.Text.Lazy as T

import Nested
import ParsePrint ( ParsePrint, parse )
import PrintUtils ( Doc, text, vcat, ($$) )

import Language.Essence
import Language.Essence.Phases.PostParse ( postParse )
import Language.EssenceLexerP ( lexAndParse )



class ReadIn a where
    readIn ::
        ( Applicative m
        , MonadError (Nested Doc) m
        , MonadWriter [Doc] m
        ) => FilePath -> T.Text -> m a

runReadIn :: ReadIn a => FilePath -> T.Text -> (Either (Nested Doc) a, [Doc])
runReadIn fp contents = runWriter $ runErrorT $ readIn fp contents

runParser' :: (ParsePrint a, Applicative m, MonadError (Nested Doc) m) => FilePath -> T.Text -> m a
runParser' fp t = do
    is <- lexAndParse (Just fp) parse t
    case is of
        []  -> throwErrorSingle "No Parse."
        [i] -> return i
        _   -> throwErrorSingle $ "Ambigious parse:" $$ vcat (map (text . show) is)

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
