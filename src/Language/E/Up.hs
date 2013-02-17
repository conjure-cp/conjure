{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
module Language.E.Up where

import Language.E

import Language.E.Up.EprimeToEssence
import Language.E.Up.Data
import Language.E.Up.ReduceSpec
import Language.E.Up.GatherInfomation
import Language.E.Up.RepresentationTree
import Language.E.Up.EvaluateTree
import Language.E.Up.AddEssenceTypes
import Language.E.Up.IO
import Language.E.Up.Debug


translateSolution
    :: FilePath         -- Input:  Essence
    -> Maybe FilePath   -- Input:  Essence param
    -> FilePath         -- Input:  Essence' model
    -> Maybe FilePath   -- Input:  Essence' param
    -> FilePath         -- Input:  Essence' solution
    -> FilePath         -- Output: Essence solution
    -> IO ()
translateSolution
    pathInEssence pathInParam
    pathInEprime pathInEprimeParam pathInEprimeSolution
    pathOutSolution = do
    return ()

