{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-} -- stupid cmdargs

module Conjure.UI where

-- conjure
import Conjure.Prelude
import Conjure.RepositoryVersion ( repositoryVersion )

-- cmdargs
import System.Console.CmdArgs hiding ( Default(..) )


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
        , essenceSolutionO :: Maybe FilePath -- essence-solution, optional, by default (eprimeSolution <-.> "solution")
        }
    | ValidateSolution
        { essence          :: FilePath       -- essence, mandatory
        , essenceParamO    :: Maybe FilePath -- essence-param, optional
        , essenceSolution  :: FilePath       -- essence-solution, mandatory, by default (eprimeSolution <-.> "solution")
        }
    deriving (Eq, Ord, Show, Data, Typeable)


ui :: UI
ui = modes
    [ Modelling
        { essence          = def   &= typ "ESSENCE_FILE"
                                   &= argPos 0
        , outputDirectory  = "conjure-output"
                                   &= typDir
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
        , strategyQ        = "i"   &= typ "STRATEGY"
                                   &= name "strategy-q"
                                   &= name "q"
                                   &= groupname "Strategies for model generation"
                                   &= explicit
                                   &= help "Strategy to use when selecting the next question to answer. \
                                           \Options: f (for first), x (for all), i (for interactive). \
                                           \The letter a (for auto) can be prepended to automatically skip \
                                           \when there is only one option at any point."
        , strategyA        = "i"   &= typ "STRATEGY"
                                   &= name "strategy-a"
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
        , essenceSolutionO = def   &= typFile
                                   &= name "essence-solution"
                                   &= explicit
                                   &= help "An Essence solution for the original problem specification.\n\
                                           \This field is optional.\n\
                                           \By default, its value will be 'foo.solution'\n\
                                           \if the Essence parameter file is named 'foo.eprime-solution'"
        }                          &= name "translate-solution"
                                   &= explicit
                                   &= help "Translation of solutions back to Essence."
    , ValidateSolution
        { essence          = def   &= typFile
                                   &= name "essence"
                                   &= explicit
                                   &= help "A problem specification in Essence"
        , essenceParamO    = def   &= typFile
                                   &= name "essence-param"
                                   &= explicit
                                   &= help "An Essence parameter.\n\
                                           \This field is optional."
        , essenceSolution  = def   &= typFile
                                   &= name "essence-solution"
                                   &= explicit
                                   &= help "An Essence solution.\n\
                                           \This field is optional."
        }                          &= name "validate-solution"
                                   &= explicit
                                   &= help "Validating a solution."
    ]                              &= program "conjure"
                                   &= summary ("Conjure, the automated constraint modelling tool.\n\
                                               \Version: " ++ repositoryVersion)
