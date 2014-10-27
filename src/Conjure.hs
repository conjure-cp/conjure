module Conjure ( getConjureMode, runConjureMode,conjureHelp ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.UI.IO
import Conjure.Language.ModelDiff ( modelDiffIO )
import Conjure.Language.Definition ( typeCheckModelIO )

import Paths_conjure_cp ( getBinDir )
import Conjure.Mode
import Language.E
import Language.E.Pipeline.ReadIn
    ( readSpecFromStdIn, readSpecFromFile
    , dropExtEssence
    )

import Conjure.UI.RefineParam ( refineParam )
import Conjure.UI.TranslateSolution ( translateSolution )

import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )
import Language.E.Pipeline.ConjureAll ( conjureWithMode )
import Language.E.Pipeline.Driver ( driverConjure, driverConjureSingle )
import Language.E.Pipeline.UniqueQuanVars ( uniqueQuanVars )
import Language.E.ValidateSolution ( validateSolution )

import System.Timeout ( timeout )
import System.CPUTime ( getCPUTime )
import Text.Printf ( printf )
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M


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
runConjureMode fullmode@(ConjureModeWithFlags mode pairs flags _rest timelimit) =
    case timelimit of
        NoTimeLimit   -> helper mode
        TimeLimit sec -> do
            putStrLn $ "Running with a timelimit of " ++ show sec ++ " seconds."
            res <- timeout (sec * 1000000) (helper mode)
            case res of
                Nothing -> do
                    cputime <- getCPUTime
                    let
                        -- cputime is returned in pico-seconds. arbitrary precision integer.
                        -- divide by 10^9 first. use arbitrary precision integer arithmetic.
                        -- do the last 10^3 division via double to get 3 significant digits after the integer part.
                        cputimeInSeconds :: Double
                        cputimeInSeconds = fromInteger (cputime `div` 1000000000) / 1000
                    putStrLn $ printf "Timed out. Total CPU time used is %.3f seconds." cputimeInSeconds
                Just () -> return ()

    where

        limit = do
            s <- M.lookup "--limit" pairs
            maybeRead s

        helper ModeUnknown = error "Unknown mode"

        helper (ModeDiff pathIn1 pathIn2) = do
            m1 <- readModelFromFile pathIn1
            m2 <- readModelFromFile pathIn2
            modelDiffIO m1 m2

        helper (ModeRefineParam pathInParam pathInEprime pathOutParam) = do
            inParam   <- readModelFromFile pathInParam
            inEprime  <- readModelPreambleFromFile pathInEprime
            either
                userErr
                (writeModel (Just pathOutParam))
                (refineParam inEprime inParam)

        helper (ModeTranslateSolution pathInParam pathInEprime pathInEprimeSolution pathOutSolution) = do
            inParam          <- case pathInParam of
                                    Nothing -> return def
                                    Just fp -> readModelFromFile fp
            inEprime         <- readModelPreambleFromFile pathInEprime
            inEprimeSolution <- readModelFromFile pathInEprimeSolution
            either
                userErr
                (writeModel (Just pathOutSolution))
                (translateSolution inEprime inParam inEprimeSolution)

        helper (ModeTypeCheck pathInp) = do
            inp <- case pathInp of
                Nothing -> readSpecFromStdIn
                Just fp -> readSpecFromFile fp
            typeCheckSpecIO inp

        helper (ModePrettify pathInp pathOut) = do
            inp <- case pathInp of
                Nothing -> readModelFromStdIn
                Just fp -> readModelFromFile fp
            typeCheckModelIO inp
            writeModel pathOut inp

        helper (ModeJSON pathInp pathOut) = do
            inp <- case pathInp of
                Nothing -> readSpecFromStdIn
                Just fp -> readSpecFromFile fp
            typeCheckSpecIO inp
            case pathOut of
                Nothing -> putStrLn     $ renderWide $ pretty $ uniqueQuanVars $ atMostOneSuchThat False inp
                Just fp -> writeFile fp $ renderWide $ pretty $ uniqueQuanVars $ atMostOneSuchThat False inp

        helper (ModeValidateSolution pathEssence pathParam pathSolution) = do
            essence  <- readSpecFromFile pathEssence
            param    <- maybe (return Nothing) (fmap Just . readSpecFromFile) pathParam
            solution <- readSpecFromFile pathSolution
            validateSolution essence param solution

        helper (ModeMultipleOutput multimode pathInEssence pathOutputDir mlimit) = do
            seed <- getStdGen
            (RulesDB ruleReprs ruleRefns) <- getRulesDB
            inEssence <- readSpecFromFile pathInEssence
            typeCheckSpecIO inEssence
            let defOutDirPath = dropExtEssence pathInEssence
                    ++ (case multimode of
                            DFAll -> "-df"
                            DFCompactParam -> "-df-compact-param"
                            DFNoChannelling -> "-df-no-channelling"
                            DFSample -> "-sample"
                       )
                    ++ (if S.member "--better" flags then "-better" else "")
            let outDirPath = fromMaybe defOutDirPath pathOutputDir
            driverConjure
                (conjureWithMode seed limit mlimit fullmode)
                outDirPath
                ruleReprs ruleRefns inEssence

        helper (ModeSingleOutput _ pathInEssence pathOutEprime) = do
            seed <- getStdGen
            (RulesDB ruleReprs ruleRefns) <- getRulesDB
            inEssence <- readSpecFromFile pathInEssence
            typeCheckSpecIO inEssence
            driverConjureSingle True False
                (Just pathOutEprime)
                (conjureWithMode
                    seed limit Nothing fullmode
                    ruleReprs ruleRefns inEssence)

typeCheckSpecIO :: Spec -> IO ()
typeCheckSpecIO spec =
    case fst $ runCompESingle "Error while type checking." $ typeCheckSpec spec of
        Left  e  -> userErr e
        Right () -> return ()

