{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-} -- stupid cmdargs

module Conjure.UI ( UI(..), ui ) where

-- conjure
import Conjure.Prelude
import Conjure.RepositoryVersion ( repositoryVersion )

-- cmdargs
import System.Console.CmdArgs hiding ( Default(..) )


data UI
    = Modelling
        { essence                    :: FilePath       -- essence, mandatory
        -- flags related to output
        , outputDirectory            :: FilePath
        , numberingStart             :: Int
        , smartFilenames             :: Bool
        -- flags related to logging
        , logLevel                   :: LogLevel
        , verboseTrail               :: Bool
        , logRuleFails               :: Bool
        , logRuleSuccesses           :: Bool
        , logRuleAttempts            :: Bool
        , logChoices                 :: Bool
        -- flags related to modelling decisions
        , strategyQ                  :: String
        , strategyA                  :: String
        , representations            :: Maybe String        -- (def: strategyA)
        , representationsFinds       :: Maybe String        -- (def: representations)
        , representationsGivens      :: Maybe String        -- (def: c)
        , representationsAuxiliaries :: Maybe String        -- (def: representations)
        , representationsQuantifieds :: Maybe String        -- (def: representations)
        , representationsCuts        :: Maybe String        -- (def: representations)
        , channelling                :: Bool
        , seed                       :: Maybe Int
        , limitModels                :: Maybe Int
        , limitTime                  :: Maybe Int
        , savedChoices               :: Maybe FilePath
        , outputBinary               :: Bool
        }
    | RefineParam
        { eprime           :: FilePath       -- eprime, mandatory
        , essenceParam     :: FilePath       -- essence-param, mandatory
        , eprimeParam      :: Maybe FilePath -- eprime-param, optional, by default (essenceParam <-.> "eprime-param")
        , logLevel         :: LogLevel
        , outputBinary     :: Bool
        , limitTime        :: Maybe Int
        }
    | TranslateSolution
        { eprime           :: FilePath       -- eprime, mandatory
        , essenceParamO    :: Maybe FilePath -- essence-param, optional
        , eprimeSolution   :: FilePath       -- eprime-solution, mandatory
        , essenceSolutionO :: Maybe FilePath -- essence-solution, optional, by default (eprimeSolution <-.> "solution")
        , logLevel         :: LogLevel
        , outputBinary     :: Bool
        , limitTime        :: Maybe Int
        }
    | ValidateSolution
        { essence          :: FilePath       -- essence, mandatory
        , essenceParamO    :: Maybe FilePath -- essence-param, optional
        , essenceSolution  :: FilePath       -- essence-solution, mandatory, by default (eprimeSolution <-.> "solution")
        , logLevel         :: LogLevel
        , outputBinary     :: Bool
        , limitTime        :: Maybe Int
        }
    | Pretty
        { essence          :: FilePath
        , logLevel         :: LogLevel
        , outputBinary     :: Bool
        , limitTime        :: Maybe Int
        }
    | Diff
        { file1            :: FilePath
        , file2            :: FilePath
        , logLevel         :: LogLevel
        , outputBinary     :: Bool
        , limitTime        :: Maybe Int
        }
    | TypeCheck
        { essence          :: FilePath
        , logLevel         :: LogLevel
        , outputBinary     :: Bool
        , limitTime        :: Maybe Int
        }
    | Split
        { essence          :: FilePath
        , outputDirectory  :: FilePath
        , logLevel         :: LogLevel
        , outputBinary     :: Bool
        , limitTime        :: Maybe Int
        }
    | SymmetryDetection
        { essence          :: FilePath
        , json             :: FilePath
        , logLevel         :: LogLevel
        , outputBinary     :: Bool
        , limitTime        :: Maybe Int
        }
    | ParameterGenerator
        { essence          :: FilePath
        , essenceOut       :: FilePath
        , logLevel         :: LogLevel
        , outputBinary     :: Bool
        , limitTime        :: Maybe Int
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
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Output directory. Generated models will be saved here.\n\
                                           \Default value: 'conjure-output'"
        , numberingStart   = 1     &= name "numbering-start"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Starting value to output files.\n\
                                           \Default value: 1"
        , smartFilenames   = False &= name "smart-filenames"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Use \"smart names\" for the models.\n\
                                           \Turned off by default.\n\
                                           \Caution: With this flag, Conjure will use the answers when producing \
                                           \a filename. It will ignore the order of questions. \
                                           \This will become a problem if anything other than 'f' is used for questions."
        , logLevel         = def   &= name "log-level"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Log level."
        , verboseTrail     = False &= name "verbose-trail"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Whether to generate verbose trails or not."
        , logRuleFails     = False &= name "log-rule-fails"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Generate logs for rule failures. (Caution: can be a lot!)"
        , logRuleSuccesses = False &= name "log-rule-successes"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Generate logs for rule applications."
        , logRuleAttempts  = False &= name "log-rule-attempts"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Generate logs for rule attempts. (Caution: can be a lot!)"
        , logChoices       = False &= name "log-choices"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Store the choices in a way that can be reused be by -al"
        , strategyQ        = "f"   &= typ "STRATEGY"
                                   &= name "strategy-q"
                                   &= name "q"
                                   &= groupname "Model generation"
                                   &= explicit
                                   &= help "Strategy to use when selecting the next question to answer. \
                                           \Options: f (for first), i (for interactive), r (for random), x (for all). \
                                           \The letter a (for auto) can be prepended to automatically skip \
                                           \when there is only one option at any point.\n\
                                           \Default value: f"
        , strategyA        = "ai"  &= typ "STRATEGY"
                                   &= name "strategy-a"
                                   &= name "a"
                                   &= groupname "Model generation"
                                   &= explicit
                                   &= help "Strategy to use when selecting an answer. Same options as strategy-q.\n\
                                           \Moreover, c (for compact) can be used to pick the most 'compact' option \
                                           \at every decision point.\n\
                                           \And, s (for sparse) can be used to pick the most 'sparse' option \
                                           \at every decision point. \
                                           \This can be particularly useful for --representations-givens\n\
                                           \ l (for follow log) tries to pick the given choices as far as possible\n\
                                           \Default value: ai"
        , representations = Nothing
                                   &= typ "STRATEGY"
                                   &= name "representations"
                                   &= groupname "Model generation"
                                   &= explicit
                                   &= help "Strategy to use when choosing a representation.\n\
                                           \Default value: same as --strategy-a"
        , representationsFinds = Nothing
                                   &= typ "STRATEGY"
                                   &= name "representations-finds"
                                   &= groupname "Model generation"
                                   &= explicit
                                   &= help "Strategy to use when choosing a representation for a decision variable.\n\
                                           \Default value: same as --representations"
        , representationsGivens = Nothing
                                   &= typ "STRATEGY"
                                   &= name "representations-givens"
                                   &= groupname "Model generation"
                                   &= explicit
                                   &= help "Strategy to use when choosing a representation for a parameter.\n\
                                           \Default value: c (for compact)"
        , representationsAuxiliaries = Nothing
                                   &= typ "STRATEGY"
                                   &= name "representations-auxiliaries"
                                   &= groupname "Model generation"
                                   &= explicit
                                   &= help "Strategy to use when choosing a representation for an auxiliary variable.\n\
                                           \Default value: same as --representations"
        , representationsQuantifieds = Nothing
                                   &= typ "STRATEGY"
                                   &= name "representations-quantifieds"
                                   &= groupname "Model generation"
                                   &= explicit
                                   &= help "Strategy to use when choosing a representation for a quantified variable.\n\
                                           \Default value: same as --representations"
        , representationsCuts = Nothing
                                   &= typ "STRATEGY"
                                   &= name "representations-cuts"
                                   &= groupname "Model generation"
                                   &= explicit
                                   &= help "Strategy to use when choosing a representation for cuts in 'branching on'.\n\
                                           \Default value: same as --representations-cuts"
        , channelling = True       &= name "channelling"
                                   &= groupname "Model generation"
                                   &= explicit
                                   &= help "Whether to produce channelled models or not.\n\
                                           \Can be true or false. (true by default)\n\
                                           \    false: Do not produce channelled models.\n\
                                           \    true : Produce channelled models."
        , seed = Nothing           &= name "seed"
                                   &= groupname "Model generation"
                                   &= explicit
                                   &= help "The seed for the random number generator."
        , limitModels = Nothing    &= name "limit-models"
                                   &= groupname "Model generation"
                                   &= explicit
                                   &= help "Maximum number of models to generate."
        , limitTime = Nothing      &= name "limit-time"
                                   &= groupname "Model generation"
                                   &= explicit
                                   &= help "Time limit in seconds. (CPU time)."
        , savedChoices     = def   &= typFile
                                   &= name "choices"
                                   &= groupname "Model generation"
                                   &= explicit
                                   &= help "Choices to use if possible for -al \
                                            \can either be a eprime file (created by --logChoices), or a json file "
        , outputBinary = False     &= name "output-binary"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Output binary files instead of text files.\n\
                                           \Conjure can read in these binary files for further processing."
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
        , logLevel         = def   &= name "log-level"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Log level."
        , limitTime = Nothing      &= name "limit-time"
                                   &= explicit
                                   &= help "Time limit in seconds. (CPU time)."
        , outputBinary = False     &= name "output-binary"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Output binary files instead of text files.\n\
                                           \Conjure can read in these binary files for further processing."
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
                                           \By default, its value will be the value of --eprime-solution, \
                                           \with all extensions dropped the extension '.solution' is added instead."
        , logLevel         = def   &= name "log-level"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Log level."
        , limitTime = Nothing      &= name "limit-time"
                                   &= explicit
                                   &= help "Time limit in seconds. (CPU time)."
        , outputBinary = False     &= name "output-binary"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Output binary files instead of text files.\n\
                                           \Conjure can read in these binary files for further processing."
        }                          &= name "translate-solution"
                                   &= explicit
                                   &= help "Translation of solutions back to Essence."
    , ValidateSolution
        { essence          = def   &= typFile
                                   &= name "essence"
                                   &= explicit
                                   &= help "A problem specification in Essence"
        , essenceParamO    = def   &= typFile
                                   &= name "param"
                                   &= explicit
                                   &= help "An Essence parameter.\n\
                                           \This field is optional."
        , essenceSolution  = def   &= typFile
                                   &= name "solution"
                                   &= explicit
                                   &= help "An Essence solution."
        , logLevel         = def   &= name "log-level"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Log level."
        , limitTime = Nothing      &= name "limit-time"
                                   &= explicit
                                   &= help "Time limit in seconds. (CPU time)."
        , outputBinary = False     &= name "output-binary"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Output binary files instead of text files.\n\
                                           \Conjure can read in these binary files for further processing."
        }                          &= name "validate-solution"
                                   &= explicit
                                   &= help "Validating a solution."
    , Pretty
        { essence = def            &= typFile
                                   &= argPos 0
        , logLevel         = def   &= name "log-level"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Log level."
        , limitTime = Nothing      &= name "limit-time"
                                   &= explicit
                                   &= help "Time limit in seconds. (CPU time)."
        , outputBinary = False     &= name "output-binary"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Output binary files instead of text files.\n\
                                           \Conjure can read in these binary files for further processing."
        }                          &= name "pretty"
                                   &= explicit
                                   &= help "Pretty print as Essence file to stdout.\n\
                                           \This mode can be used to view a binary Essence file in textual form."
    , Diff
        { file1 = def              &= typFile
                                   &= argPos 0
        , file2 = def              &= typFile
                                   &= argPos 1
        , logLevel         = def   &= name "log-level"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Log level."
        , limitTime = Nothing      &= name "limit-time"
                                   &= explicit
                                   &= help "Time limit in seconds. (CPU time)."
        , outputBinary = False     &= name "output-binary"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Output binary files instead of text files.\n\
                                           \Conjure can read in these binary files for further processing."
        }                          &= name "diff"
                                   &= explicit
                                   &= help "Diff on two Essence files. Works on models, parameters, and solutions."
    , TypeCheck
        { essence          = def   &= typ "ESSENCE_FILE"
                                   &= argPos 0
        , logLevel         = def   &= name "log-level"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Log level."
        , limitTime = Nothing      &= name "limit-time"
                                   &= explicit
                                   &= help "Time limit in seconds. (CPU time)."
        , outputBinary = False     &= name "output-binary"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Output binary files instead of text files.\n\
                                           \Conjure can read in these binary files for further processing."
        }                          &= name "type-check"
                                   &= explicit
                                   &= help "Type-checking a single Essence file."
    , Split
        { essence          = def   &= typ "ESSENCE_FILE"
                                   &= argPos 0
        , outputDirectory  = "conjure-output"
                                   &= typDir
                                   &= name "output-directory"
                                   &= name "o"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Output directory. Generated models will be saved here.\n\
                                           \Default value: 'conjure-output'"
        , logLevel         = def   &= name "log-level"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Log level."
        , limitTime = Nothing      &= name "limit-time"
                                   &= explicit
                                   &= help "Time limit in seconds. (CPU time)."
        , outputBinary = False     &= name "output-binary"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Output binary files instead of text files.\n\
                                           \Conjure can read in these binary files for further processing."
        }                          &= name "split"
                                   &= explicit
                                   &= help "Split an Essence files to various smaller files. Useful for testing."
    , SymmetryDetection
        { essence          = def   &= typ "ESSENCE_FILE"
                                   &= argPos 0
        , json             = def   &= typ "JSON_FILE"
                                   &= name "json"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Output JSON file.\n\
                                           \By default, its value will be 'foo.essence-json'\n\
                                           \if the Essence file is named 'foo.essence'"
        , logLevel         = def   &= name "log-level"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Log level."
        , limitTime = Nothing      &= name "limit-time"
                                   &= explicit
                                   &= help "Time limit in seconds. (CPU time)."
        , outputBinary = False     &= name "output-binary"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Output binary files instead of text files.\n\
                                           \Conjure can read in these binary files for further processing."
        }                          &= name "symmetry-detection"
                                   &= explicit
                                   &= help "Dump some JSON to be used as input to ferret for symmetry detection."
    , ParameterGenerator
        { essence          = def   &= typ "ESSENCE_FILE"
                                   &= argPos 0
        , essenceOut       = def   &= typ "ESSENCE_FILE"
                                   &= typFile
                                   &= name "essence-out"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Output file path."
        , logLevel         = def   &= name "log-level"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Log level."
        , limitTime = Nothing      &= name "limit-time"
                                   &= explicit
                                   &= help "Time limit in seconds. (CPU time)."
        , outputBinary = False     &= name "output-binary"
                                   &= groupname "Logging & Output"
                                   &= explicit
                                   &= help "Output binary files instead of text files.\n\
                                           \Conjure can read in these binary files for further processing."
        }                          &= name "parameter-generator"
                                   &= explicit
                                   &= help "Generate an Essence model describing the instances of the problem class \
                                           \defined in the input Essence model.\n\
                                           \An error will be printed if the model has infinitely many instances."
    ]                              &= program "conjure"
                                   &= summary ("Conjure, the automated constraint modelling tool.\n\
                                               \Version: " ++ repositoryVersion)
