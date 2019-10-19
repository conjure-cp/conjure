{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Conjure.Language.Definition
    ( forgetRepr, rangesInts
    , languageEprime
    , initInfo
    , allContextsExceptReferences

    , quantifiedVar, quantifiedVarOverDomain, auxiliaryVar
    , lambdaToFunction

    , e2c
    , nbUses

    , Model(..), LanguageVersion(..)
    , ModelInfo(..), Decision(..), TrailRewrites(..)
    , Statement(..), SearchOrder(..), Objective(..)
    , Declaration(..), FindOrGiven(..)
    , Strategy(..)
    , viewAuto, parseStrategy

    , Name(..)
    , Expression(..), ReferenceTo(..), Region(..), InBubble(..)
    , Constant(..)
    , AbstractLiteral(..)
    , AbstractPattern(..)
    , GeneratorOrCondition(..), Generator(..), generatorPat

    , ExpressionLike(..), ReferenceContainer(..)

    , extractLettings
    , tupleLitIfNeeded
    , patternToExpr
    , emptyCollectionX

    , module Conjure.Language.NameGen

    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Pretty
import Conjure.Language.AdHoc

import Conjure.Language.Name
import Conjure.Language.NameGen ( NameGen(..), NameGenState, runNameGen )
import Conjure.Language.Constant
import Conjure.Language.AbstractLiteral
import Conjure.Language.Domain
import Conjure.Language.Expression


-- aeson
import Data.Aeson ( (.=), (.:) )
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector

-- uniplate
import Data.Generics.Uniplate.Zipper ( Zipper, down, right, hole )


------------------------------------------------------------------------------------------------------------------------
-- Model ---------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data Model = Model
    { mLanguage :: LanguageVersion
    , mStatements :: [Statement]
    , mInfo :: ModelInfo
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Model
instance Hashable  Model
instance ToJSON    Model where toJSON = genericToJSON jsonOptions
instance FromJSON  Model where parseJSON = genericParseJSON jsonOptions

instance Default Model where
    def = Model def [] def

instance Pretty Model where
    pretty (Model lang stmts info) = vcat $ concat
        [ [pretty lang]
        , [""]
        , map pretty stmts
        , [""]
        , [pretty info | info /= def]
        ]

instance VarSymBreakingDescription Model where
    varSymBreakingDescription m = JSON.Object $ M.fromList
        [ ("type", JSON.String "Model")
        , ("symmetricChildren", JSON.Bool True)
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription $ mStatements m)
        ]


languageEprime :: Model -> Model
languageEprime m = m { mLanguage = LanguageVersion "ESSENCE'" [1,0] }

allContextsExceptReferences :: Zipper a Expression -> [Zipper a Expression]
allContextsExceptReferences z0 = concatMap subtreeOf (allSiblings z0)
    where
        -- the input has to be the left most
        allSiblings :: Zipper a Expression -> [Zipper a Expression]
        allSiblings z = z : maybe [] allSiblings (right z)

        subtreeOf :: Zipper a Expression -> [Zipper a Expression]
        subtreeOf z = z : case hole z of
            Reference{} -> []                                       -- don't go through a Reference
            _           -> maybe [] allContextsExceptReferences (down z)

------------------------------------------------------------------------------------------------------------------------
-- LanguageVersion -----------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data LanguageVersion = LanguageVersion Name [Int]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize LanguageVersion
instance Hashable  LanguageVersion

instance ToJSON LanguageVersion where
    toJSON (LanguageVersion t is) =
        JSON.object [ "language" .= toJSON t
                    , "version"  .= toJSON is
                    ]

instance FromJSON LanguageVersion where
    parseJSON (JSON.Object x) =
        LanguageVersion <$> x .: "language"
                        <*> x .: "version"
    parseJSON x = bug $ "Error while parsing JSON:" <++> pretty (show x)

instance Default LanguageVersion where
    def = LanguageVersion "Essence" [1,3]

instance Pretty LanguageVersion where
    pretty (LanguageVersion language version) =
        "language" <+> pretty language
                   <+> hcat (intersperse "." (map pretty version))


------------------------------------------------------------------------------------------------------------------------
-- ModelInfo -----------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data ModelInfo = ModelInfo
    { miGivens :: [Name]
    , miFinds :: [Name]
    , miLettings :: [(Name, Expression)]
    , miEnumGivens :: [Name]
    , miEnumLettings :: [Declaration]
    , miUnnameds :: [(Name, Expression)]
    , miOriginalDomains :: [(Name, Domain () Expression)]
    , miRepresentations :: [(Name, Domain HasRepresentation Expression)]
    , miRepresentationsTree :: [(Name, [Tree (Maybe HasRepresentation)])]
    , miStrategyQ :: Strategy
    , miStrategyA :: Strategy
    , miTrailCompact :: [ ( Int     -- picked question #
                          , Int     -- picked answer #
                          , Int     -- number of answers
                          ) ]
    , miTrailVerbose :: [Decision]
    , miTrailRewrites :: [TrailRewrites]
    , miNameGenState :: [(Text, Int)]
    , miNbExtraGivens :: Int -- number of extra givens Conjure added to make the domains of original givens finite
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

modelInfoJSONOptions :: JSON.Options
modelInfoJSONOptions = jsonOptions { JSON.fieldLabelModifier = onHead toLower . drop 2 }
    where onHead f (x:xs) = f x : xs
          onHead _ [] = []

instance Serialize ModelInfo
instance Hashable  ModelInfo
instance ToJSON    ModelInfo where toJSON = genericToJSON modelInfoJSONOptions
instance FromJSON  ModelInfo where parseJSON = genericParseJSON modelInfoJSONOptions

instance Default ModelInfo where
    def = ModelInfo def def def def def def def def def def def def def def def def

instance Pretty ModelInfo where
    pretty = commentLines . pretty . toJSON
        where
            commentLines :: Doc -> Doc
            commentLines
                = vcat                          -- Doc
                . (++ [""])                     -- add an empty line to the end
                . map ("$ " `mappend`)          -- comment each line
                . ("Conjure's" :)               -- add the heading
                . map pretty                    -- [Doc]
                . lines                         -- [String]
                . renderNormal                  -- to String

initInfo :: Model -> Model
initInfo model = model { mInfo = info }
    where
        info = (mInfo model)
            { miGivens = [ nm | Declaration (FindOrGiven Given nm _) <- mStatements model ]
            , miFinds  = [ nm | Declaration (FindOrGiven Find  nm _) <- mStatements model ]
            , miOriginalDomains =
                [ (nm, dom)
                | Declaration (FindOrGiven _ nm dom) <- mStatements model
                ]
            , miEnumGivens   = [ nm     | Declaration (GivenDomainDefnEnum nm)         <- mStatements model ]
            , miEnumLettings = [ d      | Declaration d@LettingDomainDefnEnum{}        <- mStatements model ]
            , miLettings     = bug "Not initialised yet: miLettings"
            , miUnnameds     = [ (nm,s) | Declaration (LettingDomainDefnUnnamed nm s)  <- mStatements model ]
            }


data Strategy
    = PickFirst         -- ^ pick the first option
    | PickAll           -- ^ keep all options
    | Interactive       -- ^ prompt the user
    | AtRandom          -- ^ pick one option at random
    | Compact           -- ^ pick the compact option
    | Sparse            -- ^ pick the most sparse option, useful for parameters (otherwise identical to PickFirst)
    | Auto Strategy
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Serialize Strategy
instance Hashable  Strategy
instance ToJSON    Strategy where toJSON = genericToJSON jsonOptions
instance FromJSON  Strategy where parseJSON = genericParseJSON jsonOptions

instance Default Strategy where def = Auto Interactive

viewAuto :: Strategy -> (Strategy, Bool)
viewAuto (Auto s) = second (const True) (viewAuto s)
viewAuto s = (s, False)

parseStrategy :: String -> Maybe Strategy
parseStrategy ['a',s] = Auto <$> parseStrategy (return s)
parseStrategy "f" = return PickFirst
parseStrategy "x" = return PickAll
parseStrategy "i" = return Interactive
parseStrategy "r" = return AtRandom
parseStrategy "c" = return Compact
parseStrategy "s" = return Sparse
parseStrategy _ = Nothing


------------------------------------------------------------------------------------------------------------------------
-- Decision ------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data Decision = Decision
    { dDescription :: [Text]
    , dNumOptions :: Maybe Int
    , dDecision :: Int
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

decisionJSONOptions :: JSON.Options
decisionJSONOptions = jsonOptions { JSON.fieldLabelModifier = map toLower . drop 1 }

instance Serialize Decision
instance Hashable  Decision
instance ToJSON    Decision where toJSON = genericToJSON decisionJSONOptions
instance FromJSON  Decision where parseJSON = genericParseJSON decisionJSONOptions


------------------------------------------------------------------------------------------------------------------------
-- TrailRewrites -------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

data TrailRewrites = TrailRewrites
    { trRule   :: Text
    , trBefore :: [Text]
    , trAfter  :: [Text]
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

trJSONOptions :: JSON.Options
trJSONOptions = jsonOptions { JSON.fieldLabelModifier = map toLower . drop 2 }

instance Serialize TrailRewrites
instance Hashable  TrailRewrites
instance ToJSON    TrailRewrites where toJSON = genericToJSON trJSONOptions
instance FromJSON  TrailRewrites where parseJSON = genericParseJSON trJSONOptions


------------------------------------------------------------------------------------------------------------------------
-- Misc ----------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

extractLettings :: Model -> [(Name, Expression)]
extractLettings model =
    [ (n, x) | Declaration (Letting n x) <- mStatements model
             , not (isDomain x)
             ]
    where isDomain Domain{} = True
          isDomain _ = False
