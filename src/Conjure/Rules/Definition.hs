{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}

module Conjure.Rules.Definition
    ( Rule(..), RuleResult(..), namedRule
    , Question(..), QuestionType(..), Answer(..)
    , LogOrModel, LogOr
    , Driver, Strategy(..), viewAuto, parseStrategy
    , Config(..)
    , isAtomic, representationOf, hasRepresentation, matchFirst
    ) where

import Conjure.Prelude
import Conjure.UserError
import Conjure.Language.Definition
import Conjure.Language.Expression.Op

import Conjure.Language.RepresentationOf
import Conjure.Process.Enumerate ( EnumerateDomain )

-- uniplate
import Data.Generics.Uniplate.Zipper ( Zipper )


type LogOr a = Either (LogLevel, Doc) a
type LogOrModel = LogOr Model

data Question = Question
    { qType       :: QuestionType
    , qHole       :: Expression
    , qAscendants :: [Expression]
    , qAnswers    :: [Answer]
    }

data QuestionType
    = ChooseRepr
    | ChooseRepr_Find
    | ChooseRepr_Given
    | ChooseRepr_Auxiliary
    | ChooseRepr_Quantified
    | ChooseRepr_Cut
    | ExpressionRefinement
    deriving (Eq, Ord, Show)

data Answer = Answer
    { aText      :: Doc
    , aAnswer    :: Expression
    , aFullModel :: Model
    , aRuleName  :: Doc
    } deriving (Show)


type Driver = (forall m . (MonadIO m, MonadFail m, MonadLog m) => [Question] -> m [Model])


data Config = Config
    { logLevel                   :: LogLevel
    , verboseTrail               :: Bool
    , logRuleFails               :: Bool
    , logRuleSuccesses           :: Bool
    , logRuleAttempts            :: Bool
    , logChoices                 :: Bool
    , strategyQ                  :: Strategy
    , strategyA                  :: Strategy
    , representations            :: Strategy
    , representationsFinds       :: Strategy
    , representationsGivens      :: Strategy
    , representationsAuxiliaries :: Strategy
    , representationsQuantifieds :: Strategy
    , representationsCuts        :: Strategy
    , outputDirectory            :: FilePath
    , channelling                :: Bool
    , representationLevels       :: Bool
    , limitModels                :: Maybe Int
    , numberingStart             :: Int
    , smartFilenames             :: Bool
    }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Default Config where
    def = Config
        { logLevel                   = LogNone
        , verboseTrail               = False
        , logRuleFails               = False
        , logRuleSuccesses           = False
        , logRuleAttempts            = False
        , logChoices                 = False
        , strategyQ                  = Interactive
        , strategyA                  = Interactive
        , representations            = Interactive
        , representationsFinds       = Interactive
        , representationsGivens      = Interactive
        , representationsAuxiliaries = Interactive
        , representationsQuantifieds = Interactive
        , representationsCuts        = Interactive
        , outputDirectory            = "conjure-output"
        , channelling                = True
        , representationLevels       = True
        , limitModels                = Nothing
        , numberingStart             = 1
        , smartFilenames             = False
        }

data RuleResult m = RuleResult
    { ruleResultDescr :: Doc                    -- describe this transformation
    , ruleResultType  :: QuestionType
    , ruleResult      :: m Expression           -- the result
    , ruleResultHook  :: Model -> m Model       -- post-application hook
    }

data Rule = Rule
    { rName  :: Doc
    , rApply
        :: forall n m . ( MonadFail n, MonadLog n, NameGen n, EnumerateDomain n, MonadReader (Zipper Model Expression) n
                                -- a fail in {n} means that the rule isn't applicable
                        , MonadFail m, MonadLog m, NameGen m, EnumerateDomain m, MonadUserError m
                                -- a fail in {m} means a bug
                        )
        => Zipper Model Expression            -- to query context
        -> Expression
        -> n [RuleResult m]
    }

namedRule
    :: Doc
    -> (forall n m . ( MonadFail n, MonadLog n, NameGen n, EnumerateDomain n, MonadReader (Zipper Model Expression) n
                     , MonadFail m, MonadLog m, NameGen m, EnumerateDomain m
                     ) => Expression -> n (Doc, m Expression))
    -> Rule
namedRule nm f = Rule
    { rName = nm
    , rApply = \ z x -> do
        (rResultDescr, rResult) <- runReaderT (f x) z
        return [RuleResult rResultDescr ExpressionRefinement rResult return]
    }


isAtomic :: Expression -> Bool
isAtomic Reference{} = True
isAtomic (Op (MkOpIndexing (OpIndexing a _))) = isAtomic a
isAtomic _ = False


matchFirst
    :: MonadFail m
    => [a]                  -- list of things to try matching on
    -> (a -> Maybe b)       -- the matcher
    -> m ( [a]              -- befores
         , b                -- the matching one
         , [a]              -- afters
         )
matchFirst = helper []
    where
        helper _ [] _ = na "matchFirst"
        helper befores (x:xs) f = case f x of
            Nothing -> helper (x:befores) xs f
            Just y  -> return (reverse befores, y, xs)
