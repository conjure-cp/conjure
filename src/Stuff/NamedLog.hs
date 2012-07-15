{-# LANGUAGE FlexibleContexts #-}

module Stuff.NamedLog where

import Control.Monad.Writer ( MonadWriter(tell) )
import Data.List ( isPrefixOf )
import Text.PrettyPrint ( Doc, (<+>), vcat, brackets, text )


data NamedLog = NamedLog String Doc
    deriving Show

suppress :: [String]
suppress = [ "patternMatch.core"
           , "patternMatch"
           , "rule-fail"
           ]

mkLog :: MonadWriter [NamedLog] m => String -> Doc -> m ()
mkLog nm _ | "debug:" `isPrefixOf` nm = return ()
mkLog nm doc = tell [ NamedLog nm doc | nm `notElem` suppress ]
-- mkLog nm doc = tell [ NamedLog nm doc ]
-- mkLog _ _ = return ()

prettyLogs :: [NamedLog] -> Doc
prettyLogs = vcat . map (\ (NamedLog nm doc) -> brackets (text nm) <+> doc )
