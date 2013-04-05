{-# LANGUAGE OverloadedStrings  #-}
module Language.E.Up(translateSolution) where

import Language.E

import Language.E.NormaliseSolution (normaliseSolutionEs)
import Language.E.Pipeline.ReadIn(writeSpec)
import Language.E.Up.EprimeToEssence(mainPure)
import Language.E.Up.IO(getSpecs)

translateSolution
    :: EssenceFP
    -> Maybe EssenceParamFP
    -> EprimeFP
    -> Maybe EprimeParamFP
    -> EprimeSolutionFP
    -> EssenceSolutionFP  --Output
    -> IO ()
translateSolution
    essence param eprime eprimeParam eprimeSolution outSolution= do
    specs <- getSpecs (eprime, eprimeSolution, essence, eprimeParam, param)

    let resSpec = Spec ("Essence",[1,3]) . listAsStatement . normaliseSolutionEs . mainPure $ specs
    writeSpec outSolution resSpec

    return ()

type EssenceFP         = FilePath
type EprimeFP          = FilePath
type EssenceParamFP    = FilePath
type EprimeParamFP     = FilePath
type EprimeSolutionFP  = FilePath
type EssenceSolutionFP = FilePath

