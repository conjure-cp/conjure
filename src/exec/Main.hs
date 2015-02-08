{-# LANGUAGE RecordWildCards #-}

module Main where

import Conjure.Prelude
import Conjure.Bug ( userErr )
import Conjure.UI ( UI(..), ui )
import Conjure.UI.IO ( readModelFromFile, readModelPreambleFromFile, writeModel )
import Conjure.UI.Model ( parseStrategy, outputModels)
import qualified Conjure.UI.Model as Config ( Config(..) )
import Conjure.UI.RefineParam ( refineParam )
import Conjure.UI.TranslateSolution ( translateSolution )
import Conjure.UI.ValidateSolution ( validateSolution )
import Conjure.UI.TypeCheck ( typeCheckModel )
import Conjure.UI.LogFollow (getAnswers)

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
            logDebug ("Command line options: " <+> pretty (groom input))
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


mainWithArgs :: (MonadIO m, MonadLog m, MonadFail m) => UI -> m ()
mainWithArgs Modelling{..} = do
    model <- readModelFromFile essence
    liftIO $ hSetBuffering stdout NoBuffering
    liftIO $ maybe (return ()) setRandomSeed seed
    answers <- case jsonChoices of
                 Just f  -> getAnswers f
                 Nothing -> return []


    let config = Config.Config
            { Config.outputDirectory         = outputDirectory
            , Config.logLevel                = logLevel
            , Config.verboseTrail            = verboseTrail
            , Config.logRuleFails            = logRuleFails
            , Config.logRuleSuccesses        = logRuleSuccesses
            , Config.logRuleAttempts         = logRuleAttempts
            , Config.strategyQ               = fromMaybe (userErr ("Not a valid strategy:" <+> pretty strategyQ))
                                                         (parseStrategy strategyQ)
                |> (\ s ->
                    if fst (viewAuto s) == Compact
                        then userErr "The Compact heuristic isn't supported for questions."
                        else s
                   )
            , Config.strategyA               = fromMaybe (userErr ("Not a valid strategy:" <+> pretty strategyA))
                                                         (parseStrategy strategyA)
            , Config.channelling             = channelling
            , Config.parameterRepresentation = parameterRepresentation
            , Config.limitModels             = if limitModels == Just 0 then Nothing else limitModels
            , Config.numberingStart          = numberingStart
            , Config.questionAnswers         = answers

            }
    outputModels config model
mainWithArgs RefineParam{..} = do
    when (null eprime      ) $ userErr "Mandatory field --eprime"
    when (null essenceParam) $ userErr "Mandatory field --essence-param"
    let outputFilename = fromMaybe (dropExtension essenceParam ++ ".eprime-param") eprimeParam
    output <- join $ refineParam
                    <$> readModelPreambleFromFile eprime
                    <*> readModelFromFile essenceParam
    writeModel (Just outputFilename) output
mainWithArgs TranslateSolution{..} = do
    when (null eprime        ) $ userErr "Mandatory field --eprime"
    when (null eprimeSolution) $ userErr "Mandatory field --eprime-solution"
    output <- join $ translateSolution
                    <$> readModelPreambleFromFile eprime
                    <*> maybe (return def) readModelFromFile essenceParamO
                    <*> readModelFromFile eprimeSolution
    let outputFilename = fromMaybe (dropExtensions eprimeSolution ++ ".solution") essenceSolutionO
    writeModel (Just outputFilename) output
mainWithArgs ValidateSolution{..} = do
    when (null essence        ) $ userErr "Mandatory field --essence"
    when (null essenceSolution) $ userErr "Mandatory field --solution"
    join $ validateSolution
        <$> readModelFromFile essence
        <*> maybe (return def) readModelFromFile essenceParamO
        <*> readModelFromFile essenceSolution
mainWithArgs Diff{..} =
    join $ modelDiffIO
        <$> readModelFromFile file1
        <*> readModelFromFile file2
mainWithArgs TypeCheck{..} =
    join $ typeCheckModel
        <$> readModelFromFile essence




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
