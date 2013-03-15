{-# LANGUAGE ScopedTypeVariables #-}

module Conjure ( getConjureMode, runConjureMode ) where

import System.Directory ( doesFileExist )
import System.Environment ( getArgs )
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as M

import Paths_conjure_cp ( getBinDir )
import Conjure.Mode
import Language.E
import Language.E.Pipeline.ReadIn
    ( readSpecFromStdIn, readSpecFromFile
    , readSpecPreambleFromFile
    , writeSpec, dropExtEssence
    )

import Language.E.NormaliseSolution ( normaliseSolution )
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )
import Language.E.Pipeline.ConjureAll ( conjureWithMode )
import Language.E.Pipeline.Driver ( driverConjure, driverConjureSingle )
import Language.E.Pipeline.RedArrow ( redArrow )
import Language.E.Up ( translateSolution )
import Language.E.ValidateSolution ( validateSolution )
import Language.E.GenerateRandomParam ( generateRandomParam )


rulesdbLoc :: IO [FilePath]
rulesdbLoc = do
    inDotCabal <- liftM (++ "/conjure.rulesdb") getBinDir
    return ["conjure.rulesdb", inDotCabal]

getRulesDB :: IO RulesDB
getRulesDB = do
    candidates <- rulesdbLoc
    let
        loopy [] = error "Cannot locate rules database file."
        loopy (c:cs) = do
            b <- doesFileExist c
            if b
                then decodeFromFile c
                else loopy cs
    loopy candidates

getConjureMode :: IO (Maybe ConjureModeWithFlags)
getConjureMode = (parseArgs . parseGenericArgs) `fmap` getArgs

runConjureMode :: ConjureModeWithFlags -> IO ()
runConjureMode (ConjureModeWithFlags mode pairs _flags _rest) = helper mode
    where

        limit = do
            s <- M.lookup "--limit" pairs
            maybeRead s

        helper ModeUnknown = error "Unknown mode"
        helper (ModeDiff pathIn1 pathIn2) = do
            s1 <- readSpecFromFile pathIn1
            let Spec _ in1 = normaliseSolution s1
            s2 <- readSpecFromFile pathIn2
            let Spec _ in2 = normaliseSolution s2
            unless ( sort (statementAsList in1) == sort (statementAsList in2) )
                $  error "Files differ."
        helper (ModeRefineParam pathInEssence pathInParam pathInEprime pathOutParam) = do
            inEssence <- readSpecPreambleFromFile pathInEssence
            inParam   <- readSpecFromFile pathInParam
            inEprime  <- readSpecPreambleFromFile pathInEprime
            inLogs    <- T.readFile (pathInEprime ++ ".logs")
            driverConjureSingle False pathOutParam
                [runCompESingle "refineParam" $ redArrow inEssence inParam inEprime inLogs]
        helper (ModeTranslateSolution pathInEssence pathInParam
                                              pathInEprime pathInEprimeParam pathInEprimeSolution
                                              pathOutSolution) =
            translateSolution pathInEssence pathInParam
                              pathInEprime pathInEprimeParam pathInEprimeSolution
                              pathOutSolution
        helper (ModeTypeCheck pathInp) = do
            inp <- case pathInp of
                Nothing -> readSpecFromStdIn
                Just fp -> readSpecFromFile fp
            typeCheckSpecIO inp
        helper (ModePrettify pathInp pathOut) = do
            inp <- case pathInp of
                Nothing -> readSpecFromStdIn
                Just fp -> readSpecFromFile fp
            typeCheckSpecIO inp
            case pathOut of
                Nothing -> printPretty  (atMostOneSuchThat inp)
                Just fp -> writeSpec fp (atMostOneSuchThat inp)
        helper (ModeValidateSolution pathEssence pathParam pathSolution) = do
            essence  <- readSpecFromFile pathEssence
            param    <- maybe (return Nothing) (fmap Just . readSpecFromFile) pathParam
            solution <- readSpecFromFile pathSolution
            validateSolution essence param solution
        helper (ModeGenerateParam pathInEssence pathOutParam) = do
            seed <- getStdGen
            inEssence <- readSpecFromFile pathInEssence
            typeCheckSpecIO inEssence
            driverConjureSingle False
                pathOutParam
                $ runCompE "generateParam"
                $ set_stdgen seed >>
                  generateRandomParam inEssence
        helper (ModeDFAll pathInEssence) = do
            seed <- getStdGen
            (ruleReprs, ruleRefns) <- getRulesDB
            inEssence <- readSpecFromFile pathInEssence
            typeCheckSpecIO inEssence
            driverConjure
                (conjureWithMode seed limit mode)
                (dropExtEssence pathInEssence)
                ruleReprs ruleRefns inEssence
        helper (ModeSingleOutput _ pathInEssence pathOutEprime) = do
            seed <- getStdGen
            (ruleReprs, ruleRefns) <- getRulesDB
            inEssence <- readSpecFromFile pathInEssence
            typeCheckSpecIO inEssence
            driverConjureSingle True
                pathOutEprime
                (conjureWithMode
                    seed limit mode
                    ruleReprs ruleRefns inEssence)

typeCheckSpecIO :: Spec -> IO ()
typeCheckSpecIO spec =
    case fst $ runCompESingle "Type checking" $ typeCheckSpec spec of
        Left  e  -> error $ renderPretty e
        Right () -> return ()

