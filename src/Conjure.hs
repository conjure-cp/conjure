{-# LANGUAGE ScopedTypeVariables #-}

module Conjure ( getConjureMode, runConjureMode,conjureHelp ) where

import System.Directory ( doesFileExist )
import System.Environment ( getArgs )
import qualified Data.Text.IO as T
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Data.Aeson.Encode.Pretty ( encodePretty )
import qualified Data.Aeson as JSON ( encode )
import qualified Data.ByteString.Lazy as BS ( writeFile )
import qualified Data.ByteString.Lazy.Char8 as BS ( putStrLn )

import Bug
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
import Language.E.GenerateParams ( generateParams )
import Language.E.GenerateRandomParam ( generateRandomParam )
import Language.E.GenerateRandomParam2 ( generateParam )


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
runConjureMode fullmode@(ConjureModeWithFlags mode pairs flags _rest) = helper mode
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
            driverConjureSingle False False pathOutParam
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
                Nothing -> putStrLn $ renderNormal (atMostOneSuchThat False inp)
                Just fp -> writeSpec fp (atMostOneSuchThat False inp)

        helper (ModeJSON b pathInp pathOut) = do
            let printer = if b then JSON.encode else encodePretty
            inp <- case pathInp of
                Nothing -> readSpecFromStdIn
                Just fp -> readSpecFromFile fp
            typeCheckSpecIO inp
            case pathOut of
                Nothing -> BS.putStrLn     (printer $ atMostOneSuchThat False inp)
                Just fp -> BS.writeFile fp (printer $ atMostOneSuchThat False inp)

        helper (ModeValidateSolution pathEssence pathParam pathSolution) = do
            essence  <- readSpecFromFile pathEssence
            param    <- maybe (return Nothing) (fmap Just . readSpecFromFile) pathParam
            solution <- readSpecFromFile pathSolution
            validateSolution essence param solution

        helper (ModeGenerateParams inEssence inEprimeDir outParamDir) = 
           generateParams  inEssence inEprimeDir outParamDir

        helper (ModeGenerateRandomParam pathInEssence pathOutParam) = do
            seed <- getStdGen
            inEssence <- readSpecFromFile pathInEssence
            typeCheckSpecIO inEssence
            driverConjureSingle False False
                pathOutParam
                $ runCompE "generateParam" (set_stdgen seed >> generateRandomParam inEssence)

        helper (ModeGenerateRandomParam2 pathInEssence pathOutParam intermediateDir basename) = do
            inEssence <- readSpecFromFile pathInEssence
            rulesDB <- getRulesDB
            typeCheckSpecIO inEssence
            param <- generateParam rulesDB inEssence intermediateDir basename
            writeSpec pathOutParam param

        helper (ModeMultipleOutput multimode pathInEssence pathOutputDir mlimit) = do
            seed <- getStdGen
            (ruleReprs, ruleRefns) <- getRulesDB
            inEssence <- readSpecFromFile pathInEssence
            typeCheckSpecIO inEssence
            let defOutDirPath = dropExtEssence pathInEssence
                    ++ (case multimode of
                            DFAll -> "-df"
                            DFCompactParam -> "-df-compact-param"
                            DFNoChannelling -> "-df-no-channelling")
                    ++ (if S.member "--better" flags then "-better" else "")
            let outDirPath = fromMaybe defOutDirPath pathOutputDir
            driverConjure
                (conjureWithMode seed limit mlimit fullmode)
                outDirPath
                ruleReprs ruleRefns inEssence

        helper (ModeSingleOutput _ pathInEssence pathOutEprime) = do
            seed <- getStdGen
            (ruleReprs, ruleRefns) <- getRulesDB
            inEssence <- readSpecFromFile pathInEssence
            typeCheckSpecIO inEssence
            driverConjureSingle True False
                pathOutEprime
                (conjureWithMode
                    seed limit Nothing fullmode
                    ruleReprs ruleRefns inEssence)

typeCheckSpecIO :: Spec -> IO ()
typeCheckSpecIO spec =
    case fst $ runCompESingle "Error while type checking." $ typeCheckSpec spec of
        Left  e  -> userErr e
        Right () -> return ()

