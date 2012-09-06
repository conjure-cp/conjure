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
           , "restoreState"
           , "Simplify"
           , "Evaluator"
           ]

buildLog :: String -> Doc -> Maybe NamedLog
buildLog nm _ | "debug:" `isPrefixOf` nm = Nothing
buildLog nm _ | nm `elem` suppress       = Nothing
buildLog nm doc = Just (NamedLog nm doc)

prettyLogs :: [NamedLog] -> Doc
prettyLogs = vcat . map (\ (NamedLog nm doc) -> brackets (text nm) <+> doc )
