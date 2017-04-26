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
        , generateNeighbourhoods          :: Bool
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
        , generateNeighbourhoods          :: Bool
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
            &= help "Output directory. Generated models will be saved here.\n\
                    \Default value: 'conjure-output'"
        , numberingStart
            = 1
            &= name "numbering-start"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Starting value to output files.\n\
                    \Default value: 1"
        , smartFilenames
            = False
            &= name "smart-filenames"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Use \"smart names\" for the models.\n\
                    \Turned off by default.\n\
                    \Caution: With this flag, Conjure will use the answers when producing \
                    \a filename. It will ignore the order of questions. \
                    \This will become a problem if anything other than 'f' is used for questions."
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
            &= help "Whether to generate verbose trails or not."
        , rewritesTrail
            = False
            &= name "rewrites-trail"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Whether to generate trails about the applied rewritings or not."
        , logRuleFails
            = False
            &= name "log-rule-fails"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate logs for rule failures. (Caution: these can be a lot!)"
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
            &= help "Generate logs for rule attempts. (Caution: these can be a lot!)"
        , logChoices
            = False
            &= name "log-choices"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Store the choices in a way that can be reused be by -al"
        , strategyQ
            = "f"
            &= typ "STRATEGY"
            &= name "strategy-q"
            &= name "q"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy to use when selecting the next question to answer. \
                    \Options: f (for first), i (for interactive), r (for random), x (for all). \
                    \The letter a (for auto) can be prepended to automatically skip \
                    \when there is only one option at any point.\n\
                    \Default value: f"
        , strategyA
            = "ai"
            &= typ "STRATEGY"
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
        , representations
            = Nothing
            &= typ "STRATEGY"
            &= name "representations"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy to use when choosing a representation.\n\
                    \Default value: same as --strategy-a"
        , representationsFinds
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-finds"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy to use when choosing a representation for a decision variable.\n\
                    \Default value: same as --representations"
        , representationsGivens
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-givens"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy to use when choosing a representation for a parameter.\n\
                    \Default value: s (for sparse)"
        , representationsAuxiliaries
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-auxiliaries"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy to use when choosing a representation for an auxiliary variable.\n\
                    \Default value: same as --representations"
        , representationsQuantifieds
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-quantifieds"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy to use when choosing a representation for a quantified variable.\n\
                    \Default value: same as --representations"
        , representationsCuts
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-cuts"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy to use when choosing a representation for cuts in 'branching on'.\n\
                    \Default value: same as --representations-cuts"
        , channelling
            = True
            &= name "channelling"
            &= groupname "Model generation"
            &= explicit
            &= help "Whether to produce channelled models or not.\n\
                    \Can be true or false. (true by default)\n\
                    \    false: Do not produce channelled models.\n\
                    \    true : Produce channelled models."
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
            &= groupname "Model generation"
            &= explicit
            &= help "Whether to produce SNS-style neighbourhood definitions or not.\n\
                    \Can be true or false. (false by default)."
        , seed
            = Nothing
            &= name "seed"
            &= groupname "Model generation"
            &= explicit
            &= help "The seed for the random number generator."
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
            &= help "Time limit in seconds (real time)."
        , savedChoices
            = def
            &= typFile
            &= name "choices"
            &= groupname "Model generation"
            &= explicit
            &= help "Choices to use if possible for -al \
                     \can either be a eprime file (created by --logChoices), or a json file "
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Conjure's output can be in multiple formats.\n\
                    \    plain : The default\n\
                    \    binary: A binary encoding of the Essence output.\n\
                    \            It can be read back in by Conjure.\n\
                    \    json  : A json encoding of the Essence output.\n\
                    \            It can be used by other tools integrating with Conjure\n\
                    \            in order to avoid having to parse textual Essence."
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width to use during pretty printing.\nDefault: 120"
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
                    \This field is optional.\n\
                    \By default, its value will be 'foo.eprime-param'\n\
                    \if the Essence parameter file is named 'foo.param'"
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
            &= help "Time limit in seconds (real time)."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Conjure's output can be in multiple formats.\n\
                    \    plain : The default\n\
                    \    binary: A binary encoding of the Essence output.\n\
                    \            It can be read back in by Conjure.\n\
                    \    json  : A json encoding of the Essence output.\n\
                    \            It can be used by other tools integrating with Conjure\n\
                    \            in order to avoid having to parse textual Essence."
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width to use during pretty printing.\nDefault: 120"
        }   &= name "translate-parameter"
            &= explicit
            &= help "Refinement of parameter files written in Essence for a \
                    \particular Essence' model.\n\
                    \The model needs to be generated by Conjure."
    , TranslateSolution
        { eprime
            = def
            &= typFile
            &= name "eprime"
            &= explicit
            &= help "An Essence' model generated by Conjure."
        , essenceParamO
            = def
            &= typFile
            &= name "essence-param"
            &= explicit
            &= help "An Essence parameter for the original problem specification.\n\
                    \This field is optional."
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
                    \This field is optional.\n\
                    \By default, its value will be the value of --eprime-solution, \
                    \with all extensions dropped the extension '.solution' is added instead."
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
            &= help "Time limit in seconds (real time)."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Conjure's output can be in multiple formats.\n\
                    \    plain : The default\n\
                    \    binary: A binary encoding of the Essence output.\n\
                    \            It can be read back in by Conjure.\n\
                    \    json  : A json encoding of the Essence output.\n\
                    \            It can be used by other tools integrating with Conjure\n\
                    \            in order to avoid having to parse textual Essence."
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width to use during pretty printing.\nDefault: 120"
        }   &= name "translate-solution"
            &= explicit
            &= help "Translation of solutions back to Essence."
    , ValidateSolution
        { essence
            = def
            &= typFile
            &= name "essence"
            &= explicit
            &= help "A problem specification in Essence"
        , essenceParamO
            = def
            &= typFile
            &= name "param"
            &= explicit
            &= help "An Essence parameter.\n\
                    \This field is optional."
        , essenceSolution
            = def
            &= typFile
            &= name "solution"
            &= explicit
            &= help "An Essence solution."
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
            &= help "Time limit in seconds (real time)."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Conjure's output can be in multiple formats.\n\
                    \    plain : The default\n\
                    \    binary: A binary encoding of the Essence output.\n\
                    \            It can be read back in by Conjure.\n\
                    \    json  : A json encoding of the Essence output.\n\
                    \            It can be used by other tools integrating with Conjure\n\
                    \            in order to avoid having to parse textual Essence."
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width to use during pretty printing.\nDefault: 120"
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
            &= help "Enable/disable solution validation. Off by default."
        , outputDirectory
            = "conjure-output"
            &= typDir
            &= name "output-directory"
            &= name "o"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Output directory. Generated models will be saved here.\n\
                    \Default value: 'conjure-output'"
        , numberingStart
            = 1
            &= name "numbering-start"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Starting value to output files.\n\
                    \Default value: 1"
        , smartFilenames
            = False
            &= name "smart-filenames"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Use \"smart names\" for the models.\n\
                    \Turned off by default.\n\
                    \Caution: With this flag, Conjure will use the answers when producing \
                    \a filename. It will ignore the order of questions. \
                    \This will become a problem if anything other than 'f' is used for questions."
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
            &= help "Whether to generate verbose trails or not."
        , rewritesTrail
            = False
            &= name "rewrites-trail"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Whether to generate trails about the applied rewritings or not."
        , logRuleFails
            = False
            &= name "log-rule-fails"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Generate logs for rule failures. (Caution: these can be a lot!)"
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
            &= help "Generate logs for rule attempts. (Caution: these can be a lot!)"
        , logChoices
            = False
            &= name "log-choices"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Store the choices in a way that can be reused be by -al"
        , strategyQ
            = "f"
            &= typ "STRATEGY"
            &= name "strategy-q"
            &= name "q"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy to use when selecting the next question to answer. \
                    \Options: f (for first), i (for interactive), r (for random), x (for all). \
                    \The letter a (for auto) can be prepended to automatically skip \
                    \when there is only one option at any point.\n\
                    \Default value: f"
        , strategyA
            = "c"
            &= typ "STRATEGY"
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
                    \Default value: c"
        , representations
            = Nothing
            &= typ "STRATEGY"
            &= name "representations"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy to use when choosing a representation.\n\
                    \Default value: same as --strategy-a"
        , representationsFinds
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-finds"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy to use when choosing a representation for a decision variable.\n\
                    \Default value: same as --representations"
        , representationsGivens
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-givens"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy to use when choosing a representation for a parameter.\n\
                    \Default value: s (for sparse)"
        , representationsAuxiliaries
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-auxiliaries"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy to use when choosing a representation for an auxiliary variable.\n\
                    \Default value: same as --representations"
        , representationsQuantifieds
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-quantifieds"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy to use when choosing a representation for a quantified variable.\n\
                    \Default value: same as --representations"
        , representationsCuts
            = Nothing
            &= typ "STRATEGY"
            &= name "representations-cuts"
            &= groupname "Model generation"
            &= explicit
            &= help "Strategy to use when choosing a representation for cuts in 'branching on'.\n\
                    \Default value: same as --representations-cuts"
        , channelling
            = True
            &= name "channelling"
            &= groupname "Model generation"
            &= explicit
            &= help "Whether to produce channelled models or not.\n\
                    \Can be true or false. (true by default)\n\
                    \    false: Do not produce channelled models.\n\
                    \    true : Produce channelled models."
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
            &= groupname "Model generation"
            &= explicit
            &= help "Whether to produce SNS-style neighbourhood definitions or not.\n\
                    \Can be true or false. (false by default)."
        , seed
            = Nothing
            &= name "seed"
            &= groupname "Model generation"
            &= explicit
            &= help "The seed for the random number generator."
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
            &= help "Time limit in seconds (real time)."
        , useExistingModels
            = []
            &= name "use-existing-models"
            &= groupname "Model generation"
            &= explicit
            &= typFile
            &= help "Takes paths of Essence' models generated beforehand.\n\
                    \If given, Conjure will skip the modelling phase and use the existing models for solving."
        , savilerowOptions
            = def
            &= name "savilerow-options"
            &= groupname "Options for other tools"
            &= explicit
            &= help "Options to be passed to Savile Row."
        , solverOptions
            = def
            &= name "solver-options"
            &= groupname "Options for other tools"
            &= explicit
            &= help "Options to be passed to the backend solver."
        , solver
            = "minion"
            &= name "solver"
            &= groupname "Options for other tools"
            &= explicit
            &= help "Backend solver to use.\n\
                    \Possible values: minion/lingeling/minisat\n\
                    \Default: minion"
        , nbSolutions
            = "1"
            &= name "number-of-solutions"
            &= groupname "General"
            &= explicit
            &= help "Number of solutions to find.\n\
                    \Pass the string \"all\" to enumerate all solutions.\n\
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
            &= help "Conjure's output can be in multiple formats.\n\
                    \    plain : The default\n\
                    \    binary: A binary encoding of the Essence output.\n\
                    \            It can be read back in by Conjure.\n\
                    \    json  : A json encoding of the Essence output.\n\
                    \            It can be used by other tools integrating with Conjure\n\
                    \            in order to avoid having to parse textual Essence."
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width to use during pretty printing.\nDefault: 120"
        }   &= name "solve"
            &= explicit
            &= help "This is a combined mode, and it is available for convenience.\n\
                    \It runs Conjure in the modelling mode followed by \
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
            &= help "Whether to normalise the names of quantified variables or not. Off by default."
        , removeUnused
            = False
            &= name "remove-unused"
            &= explicit
            &= help "Whether to remove unused declarations or not. Off by default."
        , limitTime
            = Nothing
            &= name "limit-time"
            &= groupname "General"
            &= explicit
            &= help "Time limit in seconds (real time)."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Conjure's output can be in multiple formats.\n\
                    \    plain : The default\n\
                    \    binary: A binary encoding of the Essence output.\n\
                    \            It can be read back in by Conjure.\n\
                    \    json  : A json encoding of the Essence output.\n\
                    \            It can be used by other tools integrating with Conjure\n\
                    \            in order to avoid having to parse textual Essence."
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width to use during pretty printing.\nDefault: 120"
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
            &= help "Time limit in seconds (real time)."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Conjure's output can be in multiple formats.\n\
                    \    plain : The default\n\
                    \    binary: A binary encoding of the Essence output.\n\
                    \            It can be read back in by Conjure.\n\
                    \    json  : A json encoding of the Essence output.\n\
                    \            It can be used by other tools integrating with Conjure\n\
                    \            in order to avoid having to parse textual Essence."
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width to use during pretty printing.\nDefault: 120"
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
            &= help "Time limit in seconds (real time)."
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
            &= help "Output directory. Generated models will be saved here.\n\
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
            &= help "Time limit in seconds (real time)."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Conjure's output can be in multiple formats.\n\
                    \    plain : The default\n\
                    \    binary: A binary encoding of the Essence output.\n\
                    \            It can be read back in by Conjure.\n\
                    \    json  : A json encoding of the Essence output.\n\
                    \            It can be used by other tools integrating with Conjure\n\
                    \            in order to avoid having to parse textual Essence."
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width to use during pretty printing.\nDefault: 120"
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
                    \By default, its value will be 'foo.essence-json'\n\
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
            &= help "Time limit in seconds (real time)."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Conjure's output can be in multiple formats.\n\
                    \    plain : The default\n\
                    \    binary: A binary encoding of the Essence output.\n\
                    \            It can be read back in by Conjure.\n\
                    \    json  : A json encoding of the Essence output.\n\
                    \            It can be used by other tools integrating with Conjure\n\
                    \            in order to avoid having to parse textual Essence."
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width to use during pretty printing.\nDefault: 120"
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
            &= help "Time limit in seconds (real time)."
        , outputFormat
            = def
            &= name "output-format"
            &= groupname "Logging & Output"
            &= explicit
            &= typ "FORMAT"
            &= help "Conjure's output can be in multiple formats.\n\
                    \    plain : The default\n\
                    \    binary: A binary encoding of the Essence output.\n\
                    \            It can be read back in by Conjure.\n\
                    \    json  : A json encoding of the Essence output.\n\
                    \            It can be used by other tools integrating with Conjure\n\
                    \            in order to avoid having to parse textual Essence."
        , lineWidth
            = 120
            &= name "line-width"
            &= groupname "Logging & Output"
            &= explicit
            &= help "Line width to use during pretty printing.\nDefault: 120"
        }   &= name "parameter-generator"
            &= explicit
            &= help "Generate an Essence model describing the instances of the problem class \
                    \defined in the input Essence model.\n\
                    \An error will be printed if the model has infinitely many instances."
    ]      &= program "conjure"
           &= summary (unlines [ "Conjure: The Automated Constraint Modelling Tool"
                               , "Release version " ++ showVersion version
                               , "Repository version " ++ repositoryVersion
                               ])
           &= help "The command line interface of Conjure takes a command name as the first argument \
                   \followed by more arguments depending on the command.\n\
                   \This help text gives a list of the available commands.\n\
                   \For details of a specific command, pass the --help flag after the command name.\n\
                   \For example: 'conjure translate-solution --help'"

