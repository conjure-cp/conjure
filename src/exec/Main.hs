{-# LANGUAGE RecordWildCards #-}

module Main where

import Conjure.Prelude
import Conjure.UserError ( MonadUserError(..) )
import Conjure.UI ( UI(..), ui )
import Conjure.UI.IO ( readModelFromFile, readModelPreambleFromFile, writeModel, EssenceFileMode(..) )
import Conjure.UI.Model ( parseStrategy, outputModels )
import qualified Conjure.UI.Model as Config ( Config(..) )
import Conjure.UI.RefineParam ( refineParam )
import Conjure.UI.TranslateSolution ( translateSolution )
import Conjure.UI.ValidateSolution ( validateSolution )
import Conjure.UI.TypeCheck ( typeCheckModel_StandAlone )
import Conjure.UI.LogFollow ( refAnswers )
import Conjure.UI.Split ( outputSplittedModels )
import Conjure.UI.VarSymBreaking ( outputVarSymBreaking )
import Conjure.UI.ParameterGenerator ( parameterGenerator )
import Conjure.UI.DomainPruning ( domainPruning )

import Conjure.Language.NameGen ( runNameGen )
import Conjure.Language.Pretty ( pretty )
import Conjure.Language.ModelDiff ( modelDiffIO )
import Conjure.Rules.Definition ( viewAuto, Strategy(..) )

-- base
import System.IO ( hSetBuffering, stdout, BufferMode(..) )
import System.CPUTime ( getCPUTime )
import System.Timeout ( timeout )
import Text.Printf ( printf )

-- cmdargs
import System.Console.CmdArgs ( cmdArgs )


main :: IO ()
main = do
    input <- cmdArgs ui
    let workload = runLoggerPipeIO (logLevel input) $ do
            logDebug ("Command line options: " <+> pretty (show input))
            mainWithArgs input
    case limitTime input of
        Just sec | sec > 0 -> do
            putStrLn $ "Running with a timelimit of " ++ show sec ++ " seconds."
            res <- timeout (sec * 1000000) workload
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
        _ -> workload


mainWithArgs :: (MonadIO m, MonadLog m, MonadFail m, MonadUserError m) => UI -> m ()
mainWithArgs Modelling{..} = do
    model <- readModelFromFile essence
    liftIO $ hSetBuffering stdout NoBuffering
    liftIO $ maybe (return ()) setRandomSeed seed
    case savedChoices of
        Just f  -> refAnswers f
        Nothing -> return ()

    let
        parseStrategy_ s = maybe (userErr1 ("Not a valid strategy:" <+> pretty strategyQ))
                                 return
                                 (parseStrategy s)

    config <- do
        strategyQ'                  <- parseStrategy_ strategyQ
        strategyA'                  <- parseStrategy_ strategyA
        representations'            <- maybe (return strategyA')       parseStrategy_ representations
        representationsFinds'       <- maybe (return representations') parseStrategy_ representationsFinds
        representationsGivens'      <- maybe (return representations') parseStrategy_ representationsGivens
        representationsAuxiliaries' <- maybe (return representations') parseStrategy_ representationsAuxiliaries
        representationsQuantifieds' <- maybe (return representations') parseStrategy_ representationsQuantifieds
        representationsCuts'        <- maybe (return representations') parseStrategy_ representationsCuts

        case fst (viewAuto strategyQ') of
            Compact -> userErr1 "The Compact heuristic isn't supported for questions."
            _       -> return ()

        return Config.Config
            { Config.outputDirectory            = outputDirectory
            , Config.logLevel                   = logLevel
            , Config.verboseTrail               = verboseTrail
            , Config.logRuleFails               = logRuleFails
            , Config.logRuleSuccesses           = logRuleSuccesses
            , Config.logRuleAttempts            = logRuleAttempts
            , Config.logChoices                 = logChoices
            , Config.strategyQ                  = strategyQ'
            , Config.strategyA                  = strategyA'
            , Config.representations            = representations'
            , Config.representationsFinds       = representationsFinds'
            , Config.representationsGivens      = representationsGivens'
            , Config.representationsAuxiliaries = representationsAuxiliaries'
            , Config.representationsQuantifieds = representationsQuantifieds'
            , Config.representationsCuts        = representationsCuts'
            , Config.channelling                = channelling
            , Config.limitModels                = if limitModels == Just 0 then Nothing else limitModels
            , Config.numberingStart             = numberingStart
            , Config.smartFilenames             = smartFilenames
            }
    runNameGen $ outputModels config model
mainWithArgs RefineParam{..} = do
    when (null eprime      ) $ userErr1 "Mandatory field --eprime"
    when (null essenceParam) $ userErr1 "Mandatory field --essence-param"
    let outputFilename = fromMaybe (dropExtension essenceParam ++ ".eprime-param") eprimeParam
    output <- runNameGen $ join $ refineParam
                    <$> readModelPreambleFromFile eprime
                    <*> readModelFromFile essenceParam
    writeModel (if outputBinary then BinaryEssence else PlainEssence)
               (Just outputFilename)
               output
mainWithArgs TranslateSolution{..} = do
    when (null eprime        ) $ userErr1 "Mandatory field --eprime"
    when (null eprimeSolution) $ userErr1 "Mandatory field --eprime-solution"
    output <- runNameGen $ join $ translateSolution
                    <$> readModelPreambleFromFile eprime
                    <*> maybe (return def) readModelFromFile essenceParamO
                    <*> readModelFromFile eprimeSolution
    let outputFilename = fromMaybe (dropExtension eprimeSolution ++ ".solution") essenceSolutionO
    writeModel (if outputBinary then BinaryEssence else PlainEssence)
               (Just outputFilename) output
mainWithArgs ValidateSolution{..} = do
    when (null essence        ) $ userErr1 "Mandatory field --essence"
    when (null essenceSolution) $ userErr1 "Mandatory field --solution"
    join $ validateSolution
        <$> readModelFromFile essence
        <*> maybe (return def) readModelFromFile essenceParamO
        <*> readModelFromFile essenceSolution
mainWithArgs Pretty{..} = do
    model <- readModelFromFile essence
    writeModel (if outputBinary then BinaryEssence else PlainEssence)
               Nothing model
mainWithArgs Diff{..} =
    join $ modelDiffIO
        <$> readModelFromFile file1
        <*> readModelFromFile file2
mainWithArgs TypeCheck{..} =
    void $ runNameGen $ join $ typeCheckModel_StandAlone <$> readModelFromFile essence
mainWithArgs Split{..} = do
    model <- readModelFromFile essence
    outputSplittedModels outputDirectory model
mainWithArgs SymmetryDetection{..} = do
    let jsonFilePath = if null json then essence ++ "-json" else json
    model <- readModelFromFile essence
    outputVarSymBreaking jsonFilePath model
mainWithArgs ParameterGenerator{..} = do
    when (null essenceOut) $ userErr1 "Mandatory field --essence-out"
    model  <- readModelFromFile essence
    output <- parameterGenerator model
    writeModel (if outputBinary then BinaryEssence else PlainEssence)
               (Just essenceOut)
               output
mainWithArgs DomainPruning{..} = do
    -- when (null essenceOut) $ userErr1 "Mandatory field --essence-out"
    model <- readModelFromFile essence
    domainPruning showErrors only includes model
    -- output <- domainPruning model
    -- writeModel (if outputBinary then BinaryEssence else PlainEssence)
    --            (Just essenceOut)
    --            output



-- INTERACTIVE

-- (Unless a .essence is given as argument)
-- Conjure works on problem specifications written in Essence.
-- You can load one by giving it as an argument to Conjure
-- Or by using the command `:load <filename>`

-- File parsed and type checked. Options
-- Notice: Intermediate files will be saved in <dir>
-- Conjure by default saves all intermediate files it generates.
-- Better safe than sorry.


-- (Once the model is complete)
-- Do you want to solve this model?
-- (Check if the model needs params)
-- (Unless a .param file is given as argument)
-- This problem seems to require parameters.
-- `:load <filename>`
-- Use default settings for Savile Row and Minion?
-- YES
-- NO, for SR:... for Minion:...
-- (If optimisation, wanna see intermediate solutions?)
-- (If sat, wanna see 1 solution, n solutions, all solutions, just number of solutions?)
-- Solved.
-- SR Time, CSEs, blah...
-- Minion Time, Nodes, blah...


-- conjure compact .essence > .eprime
-- conjure refineParam .eprime .param > .eprime-param
-- conjure translateSolution .eprime .eprime-solution > .solution
-- conjure compact-solve .essence .param > .solution --savilerow-options "" --minion-options ""

-- SOLVING
-- savilerow -in-eprime .eprime -in-param .eprime-param -out-minion .minion (rm everything else)
-- minion .minion > SOLUTION
-- savilerow -mode translateSolution


-- (invokes the INTERACTIVE)
-- conjure
-- conjure .essence
-- conjure .essence .param






-- INTERACTIVE MODELLING
-- Conjure works by first selecting refinements for abstract parameters and decision variables (declarations)
-- and then refining expressions depending on the domain refinements. (TODO: Expand.)

-- There are # parameters and # decision variables in this problem specification.
-- # of the parameters are abstract and # of the decision variavles are abstract,
-- hence they will require domain refinement.
-- Moreover, Conjure can explore multiple domain refinements for each parameter and decision variable.
-- Channelling constraints will be generated automatically when multiple domain refinements are used in a single model.
-- Options (Channelled or not)
-- 1. Explore all combinations
-- 2. Explore all combinations, except channelled models
-- 3. Explore all combinations, except channelled models for parameters
-- 4. I want to choose per declaration
--          For x:set of tau (occurs # times)
--              no channelling
--              full channelling
--              full channelling + one redundant refinement
--              up to n different refinements
--          ...






