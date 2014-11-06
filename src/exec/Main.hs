{-# LANGUAGE RecordWildCards #-}

module Main where

import Conjure.Prelude
import Conjure.Bug ( userErr )
import Conjure.UI ( UI(..), ui )
import Conjure.UI.IO ( readModelFromFile, writeModel )
import Conjure.UI.Model
import Conjure.UI.RefineParam ( refineParam )
import Conjure.UI.TranslateSolution ( translateSolution )
import Conjure.Language.Pretty ( pretty )
import qualified Conjure.UI.Model as Config ( Config(..) )

-- base
import System.IO ( hSetBuffering, stdout, BufferMode(..) )

-- cmdargs
import System.Console.CmdArgs ( cmdArgs )


main :: IO ()
main = do
    input <- cmdArgs ui
    -- runLoggerIO LogDebug (mainWithArgs input)
    ignoreLogs (mainWithArgs input)


mainWithArgs :: (MonadIO m, MonadLog m, MonadFail m) => UI -> m ()
mainWithArgs Modelling{..} = do
    when (null essence) $ userErr "Mandatory field --essence"
    model <- readModelFromFile essence
    liftIO $ hSetBuffering stdout NoBuffering
    let config = def
            { Config.logLevel         = logLevel
            , Config.verboseTrail     = verboseTrail
            , Config.logRuleFails     = logRuleFails
            , Config.logRuleSuccesses = logRuleSuccesses
            , Config.logRuleAttempts  = logRuleAttempts
            , Config.strategyQ        = fromMaybe (userErr ("Not a valid strategy:" <+> pretty strategyQ))
                                                  (parseStrategy strategyQ)
            , Config.strategyA        = fromMaybe (userErr ("Not a valid strategy:" <+> pretty strategyA))
                                                  (parseStrategy strategyA)
            , Config.outputDirectory  = outputDirectory
            }
    outputModels config model
mainWithArgs RefineParam{..} = do
    when (null eprime      ) $ userErr "Mandatory field --eprime"
    when (null essenceParam) $ userErr "Mandatory field --essence-param"
    let outputFilename = fromMaybe (dropExtension essenceParam ++ ".eprime-param") eprimeParam
    output <- join $ refineParam
                    <$> readModelFromFile eprime
                    <*> readModelFromFile essenceParam
    writeModel (Just outputFilename) output
mainWithArgs TranslateSolution{..} = do
    when (null eprime        ) $ userErr "Mandatory field --eprime"
    when (null eprimeSolution) $ userErr "Mandatory field --eprime-solution"
    essenceParam <- maybe (return def) readModelFromFile essenceParamO
    output <- join $ translateSolution
                    <$> readModelFromFile eprime
                    <*> pure essenceParam
                    <*> readModelFromFile eprimeSolution
    let outputFilename = fromMaybe (dropExtension eprimeSolution ++ ".solution") essenceSolution
    writeModel (Just outputFilename) output






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






