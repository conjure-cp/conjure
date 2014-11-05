{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Conjure.Prelude
import Conjure.Bug ( userErr )
import Conjure.RepositoryVersion ( repositoryVersion )
import Conjure.UI.IO ( readModelFromFile, writeModel )
import Conjure.UI.Model
import qualified Conjure.UI.Model as Config ( Config(..) )
import Conjure.UI.RefineParam ( refineParam )
import Conjure.UI.TranslateSolution ( translateSolution )
import Conjure.Language.Pretty ( pretty )

import System.IO ( hSetBuffering, stdout, BufferMode(..) )

-- cmdargs
import System.Console.CmdArgs hiding ( Default(..) )


main :: IO ()
main = do
    input <- cmdArgs ui
    -- runLoggerIO LogDebug (mainWithArgs input)
    ignoreLogs (mainWithArgs input)


data UI
    = Modelling
        { essence          :: FilePath       -- essence, mandatory
        , outputDirectory  :: FilePath
        , logLevel         :: LogLevel
        , verboseTrail     :: Bool
        , logRuleFails     :: Bool
        , logRuleSuccesses :: Bool
        , logRuleAttempts  :: Bool
        , strategyQ        :: String
        , strategyA        :: String
        }
    | RefineParam
        { eprime           :: FilePath       -- eprime, mandatory
        , essenceParam     :: FilePath       -- essence-param, mandatory
        , eprimeParam      :: Maybe FilePath -- eprime-param, optional, by default (essenceParam <-.> "eprime-param")
        }
    | TranslateSolution
        { eprime           :: FilePath       -- eprime, mandatory
        , essenceParamO    :: Maybe FilePath -- essence-param, optional
        , eprimeSolution   :: FilePath       -- eprime-solution, mandatory
        , essenceSolution  :: Maybe FilePath -- essence-solution, optional, by default (eprimeSolution <-.> "solution")
        }
    deriving (Eq, Ord, Show, Data, Typeable)


ui :: UI
ui = modes
    [ Modelling
        { essence          = def   &= typ "ESSENCE_FILE"
                                   &= argPos 0
        , outputDirectory  = def   &= typDir
                                   &= name "output-directory"
                                   &= name "o"
                                   &= groupname "Control logging/output"
                                   &= explicit
                                   &= help "Output directory. Generated models will be saved here.\n\
                                           \Default value: 'conjure-output'"
        , logLevel         = def   &= name "log-level"
                                   &= groupname "Control logging/output"
                                   &= explicit
                                   &= help "Log level."
        , verboseTrail     = False &= name "verbose-trail"
                                   &= groupname "Control logging/output"
                                   &= explicit
                                   &= help "Whether to generate verbose trails or not."
        , logRuleFails     = False &= name "log-rule-fails"
                                   &= groupname "Control logging/output"
                                   &= explicit
                                   &= help "Generate logs for rule failures. (Caution: can be a lot!)"
        , logRuleSuccesses = False &= name "log-rule-successes"
                                   &= groupname "Control logging/output"
                                   &= explicit
                                   &= help "Generate logs for rule applications."
        , logRuleAttempts  = False &= name "log-rule-attempts"
                                   &= groupname "Control logging/output"
                                   &= explicit
                                   &= help "Generate logs for rule attempts. (Caution: can be a lot!)"
        , strategyQ        = "i"   &= name "strategy-q"
                                   &= name "q"
                                   &= groupname "Strategies for model generation"
                                   &= explicit
                                   &= help "Strategy to use when selecting the next question to answer. \
                                           \Options: f (for first), x (for all), i (for interactive). \
                                           \The letter a (for auto) can be prepended to automatically skip \
                                           \when there is only one option at any point."
        , strategyA        = "i"   &= name "strategy-a"
                                   &= name "a"
                                   &= groupname "Strategies for model generation"
                                   &= explicit
                                   &= help "Strategy to use when selecting an answer. Same options as strategyQ."
        }                          &= name "modelling"
                                   &= explicit
                                   &= help "The main act. Given a problem specification in Essence, \
                                           \produce constraint programming models in Essence'."
                                   &= auto
    , RefineParam
        { eprime           = def   &= typFile
                                   &= name "eprime"
                                   &= explicit
                                   &= help "An Essence' model generated by Conjure."
        , essenceParam     = def   &= typFile
                                   &= name "essence-param"
                                   &= explicit
                                   &= help "An Essence parameter for the original problem specification."
        , eprimeParam      = def   &= typFile
                                   &= name "eprime-param"
                                   &= explicit
                                   &= help "An Essence' parameter matching the Essence' model.\n\
                                           \This field is optional.\n\
                                           \By default, its value will be 'foo.eprime-param'\n\
                                           \if the Essence parameter file is named 'foo.param'"
        }                          &= name "refine-param"
                                   &= explicit
                                   &= help "Refinement of parameter files written in Essence for a \
                                           \particular Essence' model.\n\
                                           \The model needs to be generated by Conjure."
    , TranslateSolution
        { eprime           = def   &= typFile
                                   &= name "eprime"
                                   &= explicit
                                   &= help "An Essence' model generated by Conjure."
        , essenceParamO    = def   &= typFile
                                   &= name "essence-param"
                                   &= explicit
                                   &= help "An Essence parameter for the original problem specification.\n\
                                           \This field is optional."
        , eprimeSolution   = def   &= typFile
                                   &= name "eprime-solution"
                                   &= explicit
                                   &= help "An Essence' solution for the corresponding Essence' model."
        , essenceSolution  = def   &= typFile
                                   &= name "essence-solution"
                                   &= explicit
                                   &= help "An Essence solution for the original problem specification.\n\
                                           \This field is optional.\n\
                                           \By default, its value will be 'foo.solution'\n\
                                           \if the Essence parameter file is named 'foo.eprime-solution'"
        }                          &= name "translate-solution"
                                   &= explicit
                                   &= help "Translation of solutions back to Essence."
    ]                              &= program "conjure"
                                   &= summary ("Conjure, the automated constraint modelling tool.\n\
                                               \Version: " ++ repositoryVersion)


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






