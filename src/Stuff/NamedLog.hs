module Stuff.NamedLog
    ( LogTree(..), logTreeAppend
    , NamedLog, buildLog
    , prettyLog, prettyLogs, printLogs
    ) where

import Data.Default
import Data.Hashable
import Text.PrettyPrint ( Doc, (<+>), vcat, brackets, text )
import qualified Data.Set as S
import qualified Data.DList as DList

import Stuff.Pretty ( renderPretty )



data LogTree = LTEmpty | LTSingle NamedLog | LTMultiple LogTree LogTree

logTreeToList :: LogTree -> DList.DList NamedLog
logTreeToList LTEmpty          = DList.empty
logTreeToList (LTSingle   x  ) = DList.singleton x
logTreeToList (LTMultiple x y) = logTreeToList x `DList.append` logTreeToList y

logTreeAppend :: LogTree -> LogTree -> LogTree
logTreeAppend = LTMultiple

instance Default LogTree where
    def = LTEmpty

data NamedLog = NamedLog String Doc

instance Hashable NamedLog where
    hash (NamedLog a b) = hash (a, show b)

suppress :: S.Set String
suppress = S.fromList
    [ "restoreState"
    , "patternMatch.core"
    , "patternMatch-fail"
    , "patternMatch-success"
    , "rule-fail"
    , "Evaluator"
    , "Evaluator.hasRepr"
    , "Evaluator.hasType"
    , "Evaluator.hasDomain"
    , "Evaluator.domSize"
    , "Evaluator.indices"
    , "Evaluator.replace"
    , "Evaluator.tupleEq"
    , "Evaluator.matrixEq"
    , "Simplify"
    , "missing:relationRepr"
    , "missing:typeUnify"
    , "missing:mostKnown"

    -- process indicators for conjure-repr, uncomment if you want to suppress
    -- , "representation"
    -- , "addedDecl

    -- process indicators for conjure-refn, uncomment if you want to suppress
    -- , "applied"
    -- , "simplified"
    -- , "removedDecl"

    ]

buildLog :: String -> Doc -> Maybe NamedLog
buildLog nm _ | nm `S.member` suppress = Nothing
buildLog nm doc = Just (NamedLog nm doc)

prettyLog :: NamedLog -> Doc
prettyLog (NamedLog nm doc) = brackets (text nm) <+> doc

prettyLogs :: LogTree -> Doc
prettyLogs = id
    . vcat
    . map prettyLog
    . nubKeepOrder
    . DList.toList
    . logTreeToList

printLogs :: LogTree -> IO ()
printLogs LTEmpty = return ()
printLogs logs = putStrLn $ renderPretty $ prettyLogs logs

nubKeepOrder :: Hashable a => [a] -> [a]
nubKeepOrder = go []
    where
        go :: Hashable a => [Int] -> [a] -> [a]
        go _ [] = []
        go seen (x:xs) = let hashx = hash x in
            if hashx `elem` seen
                then go seen xs
                else x : go (hashx : seen) xs

