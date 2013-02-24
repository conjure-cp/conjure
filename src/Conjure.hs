{-# LANGUAGE ScopedTypeVariables #-}

module Conjure ( getConjureMode, runConjureMode ) where

import System.Environment ( getArgs )
import qualified Data.Text.IO as T

import Paths_conjure_cp ( getBinDir )
import Conjure.Mode
import Language.E
import Language.E.Pipeline.ReadIn ( readSpecFromStdIn, readSpecFromFile, writeSpec, dropExtEssence )

import Language.E.NormaliseSolution ( normaliseSolution )
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )
import Language.E.Pipeline.ConjureAll ( conjureWithMode )
import Language.E.Pipeline.Driver ( driverConjure, driverConjureSingle )
import Language.E.Pipeline.RedArrow ( redArrow )
import Language.E.Up ( translateSolution )
import Language.E.ValidateSolution ( validateSolution )


rulesdbLoc :: IO FilePath
rulesdbLoc = liftM (++ "/conjure.rulesdb") getBinDir

getRulesDB :: IO RulesDB
getRulesDB = decodeFromFile =<< rulesdbLoc

getConjureMode :: IO (Maybe ConjureMode)
getConjureMode = (parseArgs . parseGenericArgs) `fmap` getArgs

runConjureMode :: ConjureMode -> IO ()
runConjureMode ModeUnknown = error "Unknown mode"
runConjureMode (ModeDiff pathIn1 pathIn2) = do
    s1 <- readSpecFromFile pathIn1
    let Spec _ in1 = normaliseSolution s1
    s2 <- readSpecFromFile pathIn2
    let Spec _ in2 = normaliseSolution s2
    unless ( sort (statementAsList in1) == sort (statementAsList in2) )
        $  error "Files differ."
runConjureMode (ModeRefineParam pathInEssence pathInParam pathInEprime pathOutParam) = do
    inEssence <- readSpecFromFile pathInEssence
    inParam   <- readSpecFromFile pathInParam
    inEprime  <- readSpecFromFile pathInEprime
    inLogs    <- T.readFile (pathInEprime ++ ".logs")
    driverConjureSingle False pathOutParam
        [runCompESingle "refineParam" $ redArrow inEssence inParam inEprime inLogs]
runConjureMode (ModeTranslateSolution pathInEssence pathInParam
                                      pathInEprime pathInEprimeParam pathInEprimeSolution
                                      pathOutSolution) =
    translateSolution pathInEssence pathInParam
                      pathInEprime pathInEprimeParam pathInEprimeSolution
                      pathOutSolution
runConjureMode (ModePrettify pathInp pathOut) = do
    inp <- case pathInp of
        Nothing -> readSpecFromStdIn
        Just fp -> readSpecFromFile fp
    typeCheckSpecIO inp
    case pathOut of
        Nothing -> printPretty  (atMostOneSuchThat inp)
        Just fp -> writeSpec fp (atMostOneSuchThat inp)
runConjureMode (ModeValidateSolution pathEssence pathParam pathSolution) = do
    essence  <- readSpecFromFile pathEssence
    param    <- maybe (return Nothing) (fmap Just . readSpecFromFile) pathParam
    solution <- readSpecFromFile pathSolution
    validateSolution essence param solution
runConjureMode mode@(ModeDFAll pathInEssence) = do
    seed <- getStdGen
    (ruleReprs, ruleRefns) <- getRulesDB
    inEssence <- readSpecFromFile pathInEssence
    typeCheckSpecIO inEssence
    driverConjure
        (conjureWithMode seed mode)
        (dropExtEssence pathInEssence)
        ruleReprs ruleRefns inEssence
runConjureMode mode@(ModeSingleOutput _ pathInEssence pathOutEprime) = do
    seed <- getStdGen
    (ruleReprs, ruleRefns) <- getRulesDB
    inEssence <- readSpecFromFile pathInEssence
    typeCheckSpecIO inEssence
    driverConjureSingle True
        pathOutEprime
        (conjureWithMode seed mode ruleReprs ruleRefns inEssence)

typeCheckSpecIO :: Spec -> IO ()
typeCheckSpecIO spec =
    case fst $ runCompESingle "Type checking" $ typeCheckSpec spec of
        Left  e  -> error $ renderPretty e
        Right () -> return ()

