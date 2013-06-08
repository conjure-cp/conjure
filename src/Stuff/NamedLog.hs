{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Stuff.NamedLog
    ( LogTree(..), logTreeAppend
    , NamedLog, buildLog
    , printLogs
    , nubKeepOrder, nubKeepOrderBy
    ) where

import Data.Default
import Data.Hashable
import Data.Serialize
import Text.PrettyPrint
import qualified Data.HashSet as S
import qualified Data.DList as DList
import qualified GHC.Generics

import Stuff.Pretty

#ifdef TRACELOGS
import Debug.Trace ( trace ) 
#endif


data LogTree = LTEmpty | LTSingle !NamedLog | LTMultiple !LogTree !LogTree
    deriving (Show, GHC.Generics.Generic)

instance Serialize LogTree

logTreeToList :: LogTree -> DList.DList NamedLog
logTreeToList LTEmpty          = DList.empty
logTreeToList (LTSingle   x  ) = DList.singleton x
logTreeToList (LTMultiple x y) = logTreeToList x `DList.append` logTreeToList y

logTreeAppend :: LogTree -> LogTree -> LogTree
logTreeAppend = LTMultiple

instance Default LogTree where
    def = LTEmpty

data NamedLog = NamedLog String Doc
    deriving (Show, GHC.Generics.Generic)

instance Serialize NamedLog

instance Hashable NamedLog where

suppress :: S.HashSet String
suppress = S.fromList
    [ "restoreState"
    , "patternMatch.core"
    , "patternMatch-fail"
    , "patternMatch-success"
    , "rule-fail"
    , "trying-rule"
    , "Evaluator"
    , "Evaluator.hasRepr"
    , "Evaluator.hasType"
    , "Evaluator.hasDomain"
    , "Evaluator.domSize"
    , "Evaluator.dontCare"
    , "Evaluator.indices"
    , "Evaluator.replace"
    , "Evaluator.tupleEq"
    , "Evaluator.matrixEq"
    , "Evaluator.dotOrderDecomposeForTuples"
    , "Evaluator.dotOrderDecomposeForMatrices"
    , "Evaluator.stripStructuralSingle"
    , "Evaluator.stripUnnecessaryTyped"
    , "Evaluator.unrollQuantifiers"
    , "Evaluator.instantiate"
    , "Simplify"

    , "missing:relationRepr"
    , "missing:mostKnown"
    , "missing:domainOf"

    , "gensym"
    , "from-cached"

    , "builtIn.relationRepr"
    , "builtIn.relationApply"
    , "builtIn.tupleExplode"
    , "builtIn.functionLiteralApply"
    -- , "builtIn.quantificationOverTupleDomains "

    -- process indicators for conjure-repr, uncomment if you want to suppress
    -- , "configuration"
    -- , "representation"
    -- , "addedDecl"
    -- , "addedStructuralCons"
    -- , "addedChannel"

    -- process indicators for conjure-refn, uncomment if you want to suppress
    -- , "applied"
    -- , "simplified"
    -- , "removedDecl"
    -- , "removedRefinedDecl"

    -- other process indicators
    , "noTupleUnrollIfNeeded"
    , "noTupleLiterals"
    , "noTupleDomsInQuan"
    , "noTuplesReplacement"

    , "handleInfDom"
    , "addSlicing"
    , "recordSpec"

    ]

buildLog :: String -> Doc -> Maybe NamedLog
buildLog nm _ | nm `S.member` suppress = Nothing
#ifdef TRACELOGS
buildLog nm doc = trace (renderWide $ pretty nm <+> doc) $ Just (NamedLog nm doc)
#else
buildLog nm doc = Just (NamedLog nm doc)
#endif

instance Pretty NamedLog where
    pretty (NamedLog nm doc) = brackets (text nm) <+> doc

instance Pretty LogTree where
    pretty = id
           . vcat
           . map pretty
           . nubKeepOrder
           . DList.toList
           . logTreeToList

printLogs :: LogTree -> IO ()
printLogs LTEmpty = return ()
printLogs logs = putStrLn $ renderWide $ pretty logs

nubKeepOrder :: Hashable a => [a] -> [a]
nubKeepOrder = nubKeepOrderBy id

nubKeepOrderBy :: Hashable b => (a -> b) -> [a] -> [a]
nubKeepOrderBy f = go []
    where
        go _ [] = []
        go seen (x:xs) = let hashx = hash (f x) in
            if hashx `elem` seen
                then go seen xs
                else x : go (hashx : seen) xs

instance Serialize Doc where
    put = Data.Serialize.put . renderWide
    get = do s <- Data.Serialize.get ; return $ pretty (s :: String)

instance Hashable Doc where
    hashWithSalt salt d = hashWithSalt salt (show d)

