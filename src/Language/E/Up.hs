{-# LANGUAGE OverloadedStrings  #-}
module Language.E.Up(translateSolution) where

import Language.E

import Language.E.Up.EprimeToEssence
import Language.E.Up.IO
import Language.E.NormaliseSolution (normaliseSolutionEs)

translateSolution
    :: FilePath         -- Input:  Essence
    -> Maybe FilePath   -- Input:  Essence param
    -> FilePath         -- Input:  Essence' model
    -> Maybe FilePath   -- Input:  Essence' param
    -> FilePath         -- Input:  Essence' solution
    -> FilePath         -- Output: Essence solution
    -> IO ()
translateSolution
    essence param eprime eprimeParam eprimeSolution outSolution= do
    (spec, sol, org) <- getSpecs (eprime, eprimeSolution, essence, eprimeParam, param)
    unalteredOrg     <- getSpec essence

    let resultEssence =  normaliseSolutionEs $ mainPure(spec,sol,org,unalteredOrg)
    writeEssence outSolution resultEssence

    return ()


writeEssence ::  FilePath -> [E] -> IO ()
writeEssence outFilename es = writeFile outFilename $ renderPretty
    (Spec ("ESSENCE",[1,3])  (listAsStatement es))

