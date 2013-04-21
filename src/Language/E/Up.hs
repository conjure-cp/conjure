{-# LANGUAGE OverloadedStrings  #-}
module Language.E.Up(translateSolution,translateSolution',translateSolutionM) where

import Language.E

import Language.E.NormaliseSolution (normaliseSolutionEs)
import Language.E.Pipeline.ReadIn(writeSpec)
import Language.E.Up.EprimeToEssence(mainPure)
import Language.E.Up.IO(getSpecs,getSpecsM)

translateSolution
    :: EssenceFP
    -> Maybe EssenceParamFP
    -> EprimeFP
    -> Maybe EprimeParamFP
    -> EprimeSolutionFP
    -> EssenceSolutionFP  --Output
    -> IO ()
translateSolution
    essence param eprime eprimeParam eprimeSolution outSolution= 
    translateSolution' essence param eprime eprimeParam eprimeSolution
    >>= writeSpec outSolution
    >>  return ()

translateSolution'
    :: EssenceFP
    -> Maybe EssenceParamFP
    -> EprimeFP
    -> Maybe EprimeParamFP
    -> EprimeSolutionFP
    -> IO EssenceSolution 
translateSolution' essence param eprime eprimeParam eprimeSolution= do
    specs <- getSpecs (eprime, eprimeSolution, essence, eprimeParam, param)
    return $ Spec ("Essence",[1,3]) . listAsStatement . normaliseSolutionEs . mainPure $ specs


type Essence   = Spec
type Eprime    = Spec
type ESolution = Spec
type Param = Spec
type EssenceParam = Spec

translateSolutionM
  :: Monad m =>
  Essence
  -> Maybe EssenceParam
  -> Eprime
  -> Maybe Param
  -> ESolution
  -> [Text]
  -> m Spec

translateSolutionM essence param eprime eprimeParam eprimeSolution logs= do
    specs <- getSpecsM (eprime, eprimeSolution, essence, eprimeParam, param, logs)
    return $ Spec ("Essence",[1,3]) . listAsStatement . normaliseSolutionEs . mainPure $ specs

type EssenceSolution = Spec

type EssenceFP         = FilePath
type EprimeFP          = FilePath
type EssenceParamFP    = FilePath
type EprimeParamFP     = FilePath
type EprimeSolutionFP  = FilePath
type EssenceSolutionFP = FilePath

