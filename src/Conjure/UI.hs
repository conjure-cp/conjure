{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-cse #-} -- stupid cmdargs

module Conjure.UI ( UI(..), OutputFormat(..), ui ) where

-- conjure
import Conjure.Prelude
import Conjure.RepositoryVersion ( repositoryVersion )
import Paths_conjure_cp ( version )

-- base
import Data.Version ( showVersion )

-- cmdargs
import System.Console.CmdArgs hiding ( Default(..) )


data UI
    = Modelling
        { essence                    :: FilePath            -- essence, mandatory
        -- flags related to output
        , outputDirectory            :: FilePath
        , numberingStart             :: Int
        , smartFilenames             :: Bool
        , responses                  :: String
        -- flags related to logging
        , logLevel                   :: LogLevel
        , verboseTrail               :: Bool
        , rewritesTrail              :: Bool
        , logRuleFails               :: Bool
        , logRuleSuccesses           :: Bool
        , logRuleAttempts            :: Bool
        , logChoices                 :: Bool
        -- flags related to modelling decisions
        , strategyQ                  :: String
        , strategyA                  :: String
        , representations            :: Maybe String        -- (def: strategyA)
        , representationsFinds       :: Maybe String        -- (def: representations)
        , representationsGivens      :: Maybe String        -- (def: s)
        , representationsAuxiliaries :: Maybe String        -- (def: representations)
        , representationsQuantifieds :: Maybe String        -- (def: representations)
        , representationsCuts        :: Maybe String        -- (def: representations)
        , channelling                :: Bool
        , representationLevels       :: Bool                -- (def: True)
        , generateNeighbourhoods     :: Bool                -- (def: False)
        , filterNeighbourhoods       :: [Int]               -- (def: [], meaning all)
        , seed                       :: Maybe Int
        , limitModels                :: Maybe Int
        , limitTime                  :: Maybe Int
        , savedChoices               :: Maybe FilePath
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | TranslateParameter
        { eprime                     :: FilePath            -- eprime, mandatory
        , essenceParam               :: FilePath            -- essence-param, mandatory
        , eprimeParam                :: Maybe FilePath      -- eprime-param, optional
                                                            -- by default (essenceParam <-.> "eprime-param")
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | TranslateSolution
        { eprime                     :: FilePath            -- eprime, mandatory
        , essenceParamO              :: Maybe FilePath      -- essence-param, optional
        , eprimeSolution             :: FilePath            -- eprime-solution, mandatory
        , essenceSolutionO           :: Maybe FilePath      -- essence-solution, optional
                                                            -- by default (eprimeSolution <-.> "solution")
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | ValidateSolution
        { essence                    :: FilePath            -- essence, mandatory
        , essenceParamO              :: Maybe FilePath      -- essence-param, optional
        , essenceSolution            :: FilePath            -- essence-solution, mandatory
                                                            -- by default (eprimeSolution <-.> "solution")
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | Solve
        { essence                    :: FilePath            -- essence, mandatory
        , essenceParams              :: [FilePath]
        , validateSolutionsOpt       :: Bool
        -- flags related to output
        , outputDirectory            :: FilePath
        , numberingStart             :: Int
        , smartFilenames             :: Bool
        , responses                  :: String
        -- flags related to logging
        , logLevel                   :: LogLevel
        , verboseTrail               :: Bool
        , rewritesTrail              :: Bool
        , logRuleFails               :: Bool
        , logRuleSuccesses           :: Bool
        , logRuleAttempts            :: Bool
        , logChoices                 :: Bool
        -- flags related to modelling decisions
        , strategyQ                  :: String
        , strategyA                  :: String
        , representations            :: Maybe String
        , representationsFinds       :: Maybe String
        , representationsGivens      :: Maybe String
        , representationsAuxiliaries :: Maybe String
        , representationsQuantifieds :: Maybe String
        , representationsCuts        :: Maybe String
        , channelling                :: Bool
        , representationLevels       :: Bool                -- (def: True)
        , generateNeighbourhoods     :: Bool                -- (def: False)
        , filterNeighbourhoods       :: [Int]               -- (def: [], meaning all)
        , seed                       :: Maybe Int
        , limitModels                :: Maybe Int
        , limitTime                  :: Maybe Int
        , useExistingModels          :: [FilePath]          -- [] by default, which means generate models
        -- flags for SR and Minion/Lingeling
        , savilerowOptions           :: [String]
        , solverOptions              :: [String]
        , solver                     :: String
        , nbSolutions                :: String              -- a number, or "all". by default 1
        , copySolutions              :: Bool
        -- output
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | Pretty
        { essence                    :: FilePath
        , normaliseQuantified        :: Bool
        , removeUnused               :: Bool
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | Diff
        { file1                      :: FilePath
        , file2                      :: FilePath
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | TypeCheck
        { essence                    :: FilePath
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        }
    | Split
        { essence                    :: FilePath
        , outputDirectory            :: FilePath
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | SymmetryDetection
        { essence                    :: FilePath
        , json                       :: FilePath
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    | ParameterGenerator
        { essence                    :: FilePath
        , essenceOut                 :: FilePath
        , logLevel                   :: LogLevel
        , limitTime                  :: Maybe Int
        , outputFormat               :: OutputFormat        -- Essence by default
        , lineWidth                  :: Int                 -- 120 by default
        }
    deriving (Eq, Ord, Show, Data, Typeable)


data OutputFormat = Plain | Binary | JSON
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize OutputFormat
instance Hashable  OutputFormat
instance ToJSON    OutputFormat where toJSON = genericToJSON jsonOptions
instance FromJSON  OutputFormat where parseJSON = genericParseJSON jsonOptions

instance Default OutputFormat where def = Plain


ui :: UI
ui = modes
    [ Modelling
        { essence
            = def
            &= typ "ESSENCE_FILE"
            &= argPos 0
        , outputDirectory
            = "conjure-output"
            &= typDir
            &= name "output-directory"
            &= name "o"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Where to save generated models.\n\
                    \Default value: 'conjure-output'"
        , numberingStart
            = 1
            &= name "numbering-start"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Starting value for output files.\n\
                    \Default value: 1"
        , smartFilenames
            = False
            &= name "smart-filenames"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Use \"smart names\" for models.\n\
                    \Directs Conjure to use the answers when producing \
                    \a filename and to ignore the order of questions. \
                    \Only useful if 'f' is used for questions."
        , responses
            = ""
            &= name "responses"
            &= groupname "Model generation"
            &= explicit
            &= help "A comma separated list of integers.\n\
                    \If provided, these will be used as the answers during \
                    \interactive model generation instead of prompting the user."
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , verboseTrail
            = False
            &= name "verbose-trail"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate verbose trails."
        , rewritesTrail
            = False
            &= name "rewrites-trail"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate trails about the applied rewritings."
        , logRuleFails
            = False
            &= name "log-rule-fails"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate logs for rule failures. (Caution: can be a lot!)"
        , logRuleSuccesses
            = False
            &= name "log-rule-successes"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate logs for rule applications."
        , logRuleAttempts
            = False
            &= name "log-rule-attempts"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate logs for rule attempts. (Caution: can be a lot!)"
        , logChoices
            = False
            &= name "log-choices"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Store the choices in a way that can be reused by -al"
        , strategyQ
            = "f"
            &= typ "STRATEGY"
            &= name "strategy-q"
            &= name "q"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for selecting the next question to answer. \
                    \Options: f (for first), i (for interactive), r (for random), x (for all). \
                    \Prepend a (for auto) to automatically skip \
                    \when there is only one option at any point.\n\
                    \Default value: f"
        , strategyA
            = "ai"
            &= typ "STRATEGY"
            &= name "strategy-a"
            &= name "a"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for selecting an answer. Same options as strategy-q.\n\
                    \ c picks the most 'compact' option \
                    \at every decision point.\n\
                    \ s picks the 'sparsest' option \
                    \at every decision point: \
                    \useful for --representations-givens\n\
                    \ l (for follow log) tries to pick given choices as far as possible\n\
                    \Default value: ai"
        , representations
            = Nothing
            &= typ "STRATEGY"
            &= name "representations"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation.\n\
                    \Default value: same as --strategy-a"
        , representationsFinds
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-finds"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for a decision variable.\n\
                    \Default value: same as --representations"
        , representationsGivens
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-givens"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for a parameter.\n\
                    \Default value: s (for sparse)"
        , representationsAuxiliaries
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-auxiliaries"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for an auxiliary variable.\n\
                    \Default value: same as --representations"
        , representationsQuantifieds
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-quantifieds"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for a quantified variable.\n\
                    \Default value: same as --representations"
        , representationsCuts
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-cuts"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for cuts in 'branching on'.\n\
                    \Default value: same as --representations-cuts"
        , channelling
            = True
            &= name "channelling"
            &= groupname "Model generation"
            &= explicit
            &= help "Whether to produce channelled models \
                    \(true by default).\n"
        , representationLevels
            = True
            &= name "representation-levels"
            &= groupname "Model generation"
            &= explicit
            &= help "Whether to use built-in precedence levels when choosing representations.\n\
                    \These levels are used to cut down the number of generated models.\n\
                    \Can be true or false. (true by default)"
        , generateNeighbourhoods
            = False
            &= name "generate-neighbourhoods"
            &= groupname "SNS"
            &= explicit
            &= help "Whether to produce SNS-style neighbourhood definitions or not.\n\
                    \Can be true or false. (false by default)."
        , filterNeighbourhoods
            = []
            &= name "filter-neighbourhoods"
            &= groupname "SNS"
            &= explicit
            &= help "Which SNS-style neighbourhoods definitions to produce.\n\
                    \Neighbourhoods will be numbered from 1..n, and this argument is a list of integers.\
                    \Conjure will generate all neighbourhoods by default."
        , seed
            = Nothing
            &= name "seed"
            &= groupname "Model generation"
            &= explicit
            &= help "Random number generator seed."
        , limitModels
            = Nothing
            &= name "limit-models"
            &= groupname "Model generation"
            &= explicit
            &= help "Maximum number of models to generate."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , savedChoices
            = def
            &= typFile
            &= name "choices"
            &= groupname "Model generation"
            &= explicit
            &= help "Choices to use for -al, \
                     \either an eprime file (created by --log-choices), or a json file."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output.\n\
                    \    plain : default\n\
                    \    binary: can be read by Conjure\n\
                    \    json  : use to avoid parsing\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "modelling"
            &= explicit
            &= help "The main act. Given a problem specification in Essence, \
                    \produce constraint programming models in Essence'."
            &= auto
    , TranslateParameter
        { eprime
            = def
            &= typFile
            &= name "eprime"
            &= explicit
            &= help "An Essence' model generated by Conjure."
        , essenceParam
            = def
            &= typFile
            &= name "essence-param"
            &= explicit
            &= help "An Essence parameter for the original problem specification."
        , eprimeParam
            = def
            &= typFile
            &= name "eprime-param"
            &= explicit
            &= help "An Essence' parameter matching the Essence' model.\n\
                    \Default is 'foo.eprime-param' \
                    \if the Essence parameter file is named 'foo.param'."
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output.\n\
                    \    plain : default\n\
                    \    binary: can be read by Conjure\n\
                    \    json  : use to avoid parsing\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "translate-parameter"
            &= explicit
            &= help "Refinement of Essence parameter files for a \
                    \particular Essence' model.\n\
                    \The model needs to be generated by Conjure."
    , TranslateSolution
        { eprime
            = def
            &= typFile
            &= name "eprime"
            &= explicit
            &= help "An Essence' model generated by Conjure.\n\
                     \Mandatory."
        , essenceParamO
            = def
            &= typFile
            &= name "essence-param"
            &= explicit
            &= help "An Essence parameter for the original problem specification.\n\
                     \Mandatory."
        , eprimeSolution
            = def
            &= typFile
            &= name "eprime-solution"
            &= explicit
            &= help "An Essence' solution for the corresponding Essence' model."
        , essenceSolutionO
            = def
            &= typFile
            &= name "essence-solution"
            &= explicit
            &= help "An Essence solution for the original problem specification.\n\
                    \By default, its value is the value of --eprime-solution \
                    \with extensions replaced by '.solution'."
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output.\n\
                    \    plain : default\n\
                    \    binary: can be read by Conjure\n\
                    \    json  : use to avoid parsing\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "translate-solution"
            &= explicit
            &= help "Translation of solutions back to Essence."
    , ValidateSolution
        { essence
            = def
            &= typFile
            &= name "essence"
            &= explicit
            &= help "Problem specification in Essence."
        , essenceParamO
            = def
            &= typFile
            &= name "param"
            &= explicit
            &= help "Essence parameter file."
        , essenceSolution
            = def
            &= typFile
            &= name "solution"
            &= explicit
            &= help "Essence solution."
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output.\n\
                    \    plain : default\n\
                    \    binary: can be read by Conjure\n\
                    \    json  : use to avoid parsing\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "validate-solution"
            &= explicit
            &= help "Validating a solution."
    , Solve
        { essence
            = def
            &= typ "ESSENCE_FILE"
            &= argPos 0
        , essenceParams
            = []
            &= typ "PARAMETER_FILE(s)"
            &= args
        , validateSolutionsOpt
            = False
            &= name "validate-solutions"
            &= groupname "General"
            &= explicit
            &= help "Enable solution validation."
        , outputDirectory
            = "conjure-output"
            &= typDir
            &= name "output-directory"
            &= name "o"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Where to save generated models.\n\
                    \Default value: 'conjure-output'"
        , numberingStart
            = 1
            &= name "numbering-start"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Starting value for output files.\n\
                    \Default value: 1"
        , smartFilenames
            = False
            &= name "smart-filenames"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Use \"smart names\" for models.\n\
                    \Directs Conjure to use the answers when producing \
                    \a filename and to ignore the order of questions. \
                    \Only useful if 'f' is used for questions."
        , responses
            = ""
            &= name "responses"
            &= groupname "Model generation"
            &= explicit
            &= help "A comma separated list of integers.\n\
                    \If provided, these will be used as the answers during \
                    \interactive model generation instead of prompting the user."
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , verboseTrail
            = False
            &= name "verbose-trail"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate verbose trails."
        , rewritesTrail
            = False
            &= name "rewrites-trail"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate trails about the applied rewritings."
        , logRuleFails
            = False
            &= name "log-rule-fails"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate logs for rule failures. (Caution: can be a lot!)"
        , logRuleSuccesses
            = False
            &= name "log-rule-successes"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate logs for rule applications."
        , logRuleAttempts
            = False
            &= name "log-rule-attempts"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate logs for rule attempts. (Caution: can be a lot!)"
        , logChoices
            = False
            &= name "log-choices"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Store the choices in a way that can be reused by -al"
        , strategyQ
            = "f"
            &= typ "STRATEGY"
            &= name "strategy-q"
            &= name "q"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for selecting the next question to answer. \
                    \Options: f (for first), i (for interactive), r (for random), x (for all). \
                    \Prepend a (for auto) to automatically skip \
                    \when there is only one option at any point.\n\
                    \Default value: f"
        , strategyA
            = "c"
            &= typ "STRATEGY"
            &= name "strategy-a"
            &= name "a"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for selecting an answer. Same options as strategy-q.\n\
                    \ c picks the most 'compact' option \
                    \at every decision point.\n\
                    \ s picks the 'sparsest' option \
                    \at every decision point: \
                    \useful for --representations-givens\n\
                    \ l (for follow log) tries to pick given choices as far as possible\n\
                    \Default value: c"
        , representations
            = Nothing
            &= typ "STRATEGY"
            &= name "representations"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation.\n\
                    \Default value: same as --strategy-a"
        , representationsFinds
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-finds"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for a decision variable.\n\
                    \Default value: same as --representations"
        , representationsGivens
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-givens"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for a parameter.\n\
                    \Default value: s (for sparse)"
        , representationsAuxiliaries
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-auxiliaries"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for an auxiliary variable.\n\
                    \Default value: same as --representations"
        , representationsQuantifieds
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-quantifieds"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for a quantified variable.\n\
                    \Default value: same as --representations"
        , representationsCuts
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-cuts"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy for choosing a representation for cuts in 'branching on'.\n\
                    \Default value: same as --representations-cuts"
        , channelling
            = True
            &= name "channelling"
            &= groupname "Model generation"
            &= explicit
            &= help "Whether to produce channelled models \
                    \(true by default).\n"
        , representationLevels
            = True
            &= name "representation-levels"
            &= groupname "Model generation"
            &= explicit
            &= help "Whether to use built-in precedence levels when choosing representations.\n\
                    \These levels are used to cut down the number of generated models.\n\
                    \Can be true or false. (true by default)"
        , generateNeighbourhoods
            = False
            &= name "generate-neighbourhoods"
            &= groupname "SNS"
            &= explicit
            &= help "Whether to produce SNS-style neighbourhood definitions or not.\n\
                    \Can be true or false. (false by default)."
        , filterNeighbourhoods
            = []
            &= name "filter-neighbourhoods"
            &= groupname "SNS"
            &= explicit
            &= help "Which SNS-style neighbourhoods definitions to produce.\n\
                    \Neighbourhoods will be numbered from 1..n, and this argument is a list of integers.\
                    \Conjure will generate all neighbourhoods by default."
        , seed
            = Nothing
            &= name "seed"
            &= groupname "Model generation"
            &= explicit
            &= help "Random number generator seed."
        , limitModels
            = Nothing
            &= name "limit-models"
            &= groupname "Model generation"
            &= explicit
            &= help "Maximum number of models to generate."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , useExistingModels
            = []
            &= name "use-existing-models"
            &= groupname "Model generation"
            &= explicit
            &= typFile
            &= help "Paths of Essence' models generated beforehand. \
                    \If given, Conjure skips the modelling phase and uses existing models for solving."
        , savilerowOptions
            = def
            &= name "savilerow-options"
            &= groupname "Options for other tools"
            &= explicit
            &= help "Options passed to Savile Row."
        , solverOptions
            = def
            &= name "solver-options"
            &= groupname "Options for other tools"
            &= explicit
            &= help "Options passed to the backend solver."
        , solver
            = "minion"
            &= name "solver"
            &= groupname "Options for other tools"
            &= explicit
            &= help "Backend solver. \
                    \Possible values: minion/lingeling/minisat\n\
                    \Default: minion"
        , nbSolutions
            = "1"
            &= name "number-of-solutions"
            &= groupname "General"
            &= explicit
            &= help "Number of solutions to find; \
                    \\"all\" enumerates all solutions.\n\
                    \Default: 1"
        , copySolutions
            = True
            &= name "copy-solutions"
            &= groupname "General"
            &= explicit
            &= help "Whether to place a copy of solution(s) next to the Essence file or not.\n\
                    \Default: on"
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output.\n\
                    \    plain : default\n\
                    \    binary: can be read by Conjure\n\
                    \    json  : use to avoid parsing\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "solve"
            &= explicit
            &= help "A combined mode for convenience.\n\
                    \Runs Conjure in modelling mode followed by \
                    \parameter translation if required, \
                    \then Savile Row + Minion to solve, and \
                    \then solution translation."
    , Pretty
        { essence
            = def
            &= typFile
            &= argPos 0
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , normaliseQuantified
            = False
            &= name "normalise-quantified"
            &= explicit
            &= help "Normalise the names of quantified variables."
        , removeUnused
            = False
            &= name "remove-unused"
            &= explicit
            &= help "Remove unused declarations."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output.\n\
                    \    plain : default\n\
                    \    binary: can be read by Conjure\n\
                    \    json  : use to avoid parsing\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "pretty"
            &= explicit
            &= help "Pretty print as Essence file to stdout.\n\
                    \This mode can be used to view a binary Essence file in textual form."
    , Diff
        { file1
            = def
            &= typFile
            &= argPos 0
        , file2
            = def
            &= typFile
            &= argPos 1
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output.\n\
                    \    plain : default\n\
                    \    binary: can be read by Conjure\n\
                    \    json  : use to avoid parsing\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "diff"
            &= explicit
            &= help "Diff on two Essence files. Works on models, parameters, and solutions."
    , TypeCheck
        { essence
            = def
            &= typ "ESSENCE_FILE"
            &= argPos 0
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        }   &= name "type-check"
            &= explicit
            &= help "Type-checking a single Essence file."
    , Split
        { essence
            = def
            &= typ "ESSENCE_FILE"
            &= argPos 0
        , outputDirectory
            = "conjure-output"
            &= typDir
            &= name "output-directory"
            &= name "o"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Where to save generated models.\n\
                    \Default value: 'conjure-output'"
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output.\n\
                    \    plain : default\n\
                    \    binary: can be read by Conjure\n\
                    \    json  : use to avoid parsing\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "split"
            &= explicit
            &= help "Split an Essence file to various smaller files. Useful for testing."
    , SymmetryDetection
        { essence
            = def
            &= typ "ESSENCE_FILE"
            &= argPos 0
        , json
            = def
            &= typ "JSON_FILE"
            &= name "json"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Output JSON file.\n\
                    \Default is 'foo.essence-json'\n\
                    \if the Essence file is named 'foo.essence'"
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output.\n\
                    \    plain : default\n\
                    \    binary: can be read by Conjure\n\
                    \    json  : use to avoid parsing\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "symmetry-detection"
            &= explicit
            &= help "Dump some JSON to be used as input to ferret for symmetry detection."
    , ParameterGenerator
        { essence
            = def
            &= typ "ESSENCE_FILE"
            &= argPos 0
        , essenceOut
            = def
            &= typ "ESSENCE_FILE"
            &= typFile
            &= name "essence-out"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Output file path."
        , logLevel
            = def
            &= name "log-level"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Log level."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Limit in seconds of real time."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Format to use for output.\n\
                    \    plain : default\n\
                    \    binary: can be read by Conjure\n\
                    \    json  : use to avoid parsing\n"
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width for pretty printing.\nDefault: 120"
        }   &= name "parameter-generator"
            &= explicit
            &= help "Generate an Essence model describing the instances of the problem class \
                    \defined in the input Essence model.\n\
                    \An error will be printed if the model has infinitely many instances."
    ]      &= program "conjure"
           &= helpArg [explicit, name "help"]
           &= versionArg [explicit, name "version"]
           &= summary (unlines [ "Conjure: The Automated Constraint Modelling Tool"
                               , "Release version " ++ showVersion version
                               , "Repository version " ++ repositoryVersion
                               ])
           &= help "The command line interface of Conjure takes a command name as the first argument \
                   \followed by more arguments depending on the command.\n\
                   \This help text gives a list of the available commands.\n\
                   \For details of a command, pass the --help flag after the command name.\n\
                   \For example: 'conjure translate-solution --help'"

