{-# LANGUAGE FlexibleContexts #-}

module Stuff.NamedLog
    ( NamedLog, buildLog
    , prettyLog, prettyLogs, printLogs
    ) where

import Control.Monad ( unless )
import Data.Hashable
import Text.PrettyPrint ( Doc, (<+>), vcat, brackets, text )
import qualified Data.Set as S
import qualified Data.DList as DList

import Stuff.Pretty ( renderPretty )


data NamedLog = NamedLog String Doc
    deriving Show

instance Hashable NamedLog where
    hash (NamedLog a b) = hash (a, show b)

suppress :: S.Set String
suppress = S.fromList
    [ "restoreState"
    , "patternMatch.core"
    , "patternMatch"
    , "rule-fail"
    , "Simplify"
    , "Evaluator"
    , "Evaluator.hasType"
    , "Evaluator.hasDomain"
    , "Evaluator.hasRepr"
    , "Evaluator.domSize"
    , "Evaluator.indices"
    , "Evaluator.replace"
    , "missing:relationRepr"
    , "missing:typeUnify"
    , "missing:mostKnown"

    -- process indicators for conjure-repr, uncomment if you want to suppress
    -- , "representation"

    -- process indicators for conjure-refn, uncomment if you want to suppress
    -- , "applied"
    -- , "simplified"
    -- , "removed"

    ]

buildLog :: String -> Doc -> Maybe NamedLog
buildLog nm _ | nm `S.member` suppress = Nothing
buildLog nm doc = Just (NamedLog nm doc)

prettyLog :: NamedLog -> Doc
prettyLog (NamedLog nm doc) = brackets (text nm) <+> doc

prettyLogs :: DList.DList NamedLog -> Doc
prettyLogs = id
    . vcat
    . map prettyLog
    . nubKeepOrder
    . DList.toList

printLogs :: DList.DList NamedLog -> IO ()
printLogs logs =
    unless (null $ DList.toList logs)
        $ putStrLn $ renderPretty $ prettyLogs logs

nubKeepOrder :: Hashable a => [a] -> [a]
nubKeepOrder = go []
    where
        go :: Hashable a => [Int] -> [a] -> [a]
        go _ [] = []
        go seen (x:xs) = let hashx = hash x in
            if hashx `elem` seen
                then go seen xs
                else x : go (hashx : seen) xs
