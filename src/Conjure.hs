{-# LANGUAGE ScopedTypeVariables #-}

module Conjure ( getConjureMode, runConjureMode ) where

import System.Environment ( getArgs )
import qualified Data.Text.IO as T

import Paths_conjure_cp ( getBinDir )
import Conjure.Mode
import Language.E
import Language.E.Pipeline.ReadIn ( readSpecFromFile, writeSpec, dropExtEssence )

-- for RefineParam
import Language.E.Pipeline.RedArrow ( redArrow )

-- for DFAll
import Language.E.Pipeline.ConjureAll
    ( conjureAllPure
    , conjureRandomPure
    , conjureWithMode
    )
import Language.E.Pipeline.Driver ( driverConjure )


rulesdbLoc :: IO FilePath
rulesdbLoc = liftM (++ "/conjure.rulesdb") getBinDir

getConjureMode :: IO (Maybe ConjureMode)
getConjureMode = (parseArgs . parseGenericArgs) `fmap` getArgs

runConjureMode :: ConjureMode -> IO ()
runConjureMode (ModeRefineParam inEssence' inParam' inEprime' outParam') = do
    inEssence <- readSpecFromFile inEssence'
    inParam   <- readSpecFromFile inParam'
    inEprime  <- readSpecFromFile inEprime'
    inLogs    <- T.readFile (inEprime' ++ ".logs")
    outParam  <- handleInIOSingle
                    $ runCompESingle "refineParam"
                    $ redArrow inEssence inParam inEprime inLogs
    putStrLn $ "Generating file: " ++ outParam'
    writeSpec outParam' outParam
runConjureMode (ModePrettify inp out) = do
    inp' <- readSpecFromFile inp
    writeSpec out inp'
runConjureMode mode@(ModeDFAll inEssence') = do
    (ruleReprs, ruleRefns) :: RulesDB <- decodeFromFile =<< rulesdbLoc
    inEssence <- readSpecFromFile inEssence'
    driverConjure
        (conjureAllPure mode)
        (dropExtEssence inEssence')
        ruleReprs ruleRefns inEssence
runConjureMode mode@(ModeBest inEssence') = do
    (ruleReprs, ruleRefns) :: RulesDB <- decodeFromFile =<< rulesdbLoc
    inEssence <- readSpecFromFile inEssence'
    driverConjure
        (conjureWithMode mode)
        (dropExtEssence inEssence')
        ruleReprs ruleRefns inEssence
runConjureMode mode@(ModeRandom inEssence') = do
    seed <- getStdGen
    (ruleReprs, ruleRefns) :: RulesDB <- decodeFromFile =<< rulesdbLoc
    inEssence <- readSpecFromFile inEssence'
    driverConjure
        (conjureRandomPure seed mode)
        (dropExtEssence inEssence')
        ruleReprs ruleRefns inEssence
runConjureMode mode@(ModeFirst inEssence') = do
    (ruleReprs, ruleRefns) :: RulesDB <- decodeFromFile =<< rulesdbLoc
    inEssence <- readSpecFromFile inEssence'
    driverConjure
        (conjureWithMode mode)
        (dropExtEssence inEssence')
        ruleReprs ruleRefns inEssence
runConjureMode mode@(ModeSmallest inEssence') = do
    (ruleReprs, ruleRefns) :: RulesDB <- decodeFromFile =<< rulesdbLoc
    inEssence <- readSpecFromFile inEssence'
    driverConjure
        (conjureWithMode mode)
        (dropExtEssence inEssence')
        ruleReprs ruleRefns inEssence


