{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.Language.Definition
    ( forgetRepr, rangesInts
    , languageEprime

    , lambdaToFunction

    , Model(..), LanguageVersion(..)
    , ModelInfo(..), Decision(..)
    , Statement(..), Objective(..)
    , Declaration(..), FindOrGiven(..)

    , Name(..)
    , Expression(..)
    , Constant(..)
    , AbstractLiteral(..)
    , AbstractPattern(..)

    , Domain(..), Range(..)
    , DomainDefn(..), DomainDefnEnum(..), DomainDefnUnnamed(..)

    , SetAttr(..)
    , DomainAttributes(..), DomainAttribute(..)

    , HasRepresentation(..)

    , Type(..)

    , ExpressionLike(..)

    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Stuff.Pretty ( (<++>), pretty )

-- base
import GHC.Generics ( Generic )

-- text
import qualified Data.Text as T

-- aeson
import Data.Aeson ( ToJSON(..), (.=), FromJSON(..), (.:) )
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON

-- QuickCheck
import Test.QuickCheck ( Arbitrary(..), choose, oneof, vectorOf, sized )

-- uniplate
import Data.Generics.Uniplate.Data ( transform )


data Model = Model
    { mLanguage :: LanguageVersion
    , mStatements :: [Statement]
    , mInfo :: ModelInfo
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Model
instance Hashable Model
instance ToJSON Model where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON Model where parseJSON = JSON.genericParseJSON jsonOptions


instance Default Model where
    def = Model def [] def

languageEprime :: Model -> Model
languageEprime m = m { mLanguage = LanguageVersion "ESSENCE'" [1,0] }


data LanguageVersion = LanguageVersion Name [Int]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize LanguageVersion
instance Hashable LanguageVersion

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


data Statement
    = Declaration Declaration
    | SearchOrder [Name]
    | Where [Expression]
    | Objective Objective Expression
    | SuchThat [Expression]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Statement
instance Hashable Statement
instance ToJSON Statement where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON Statement where parseJSON = JSON.genericParseJSON jsonOptions


data Objective = Minimising | Maximising
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Objective
instance Hashable Objective
instance ToJSON Objective where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON Objective where parseJSON = JSON.genericParseJSON jsonOptions


data Declaration
    = FindOrGiven FindOrGiven Name (Domain () Expression)
    | Letting Name Expression
    | LettingDomainDefn DomainDefn
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Declaration
instance Hashable Declaration
instance ToJSON Declaration where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON Declaration where parseJSON = JSON.genericParseJSON jsonOptions


data FindOrGiven = Find | Given
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize FindOrGiven
instance Hashable FindOrGiven
instance ToJSON FindOrGiven where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON FindOrGiven where parseJSON = JSON.genericParseJSON jsonOptions



data ModelInfo = ModelInfo
    { miGivens :: [Name]
    , miFinds :: [Name]
    , miRepresentations :: [(Name, Domain HasRepresentation Expression)]
    , miTrail :: [Decision]
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

jsonOptions :: JSON.Options
jsonOptions = JSON.defaultOptions
    { JSON.allNullaryToStringTag = True
    , JSON.omitNothingFields = True
    , JSON.sumEncoding = JSON.ObjectWithSingleField
    }

modelInfoJSONOptions :: JSON.Options
modelInfoJSONOptions = jsonOptions { JSON.fieldLabelModifier = map toLower . drop 2 }

instance Serialize ModelInfo
instance Hashable ModelInfo
instance ToJSON ModelInfo where toJSON = JSON.genericToJSON modelInfoJSONOptions
instance FromJSON ModelInfo where parseJSON = JSON.genericParseJSON modelInfoJSONOptions

instance Default ModelInfo where
    def = ModelInfo def def def def


data Decision = Decision
    { dDescription :: [Text]
    , dOptions :: [Int]
    , dDecision :: Int
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

decisionJSONOptions :: JSON.Options
decisionJSONOptions = jsonOptions { JSON.fieldLabelModifier = map toLower . drop 1 }

instance Serialize Decision
instance Hashable Decision
instance ToJSON Decision where toJSON = JSON.genericToJSON decisionJSONOptions
instance FromJSON Decision where parseJSON = JSON.genericParseJSON decisionJSONOptions


newtype Name = Name Text
    deriving (Eq, Ord, Show, Data, Typeable, Generic, IsString, Serialize, Hashable, ToJSON, FromJSON, Monoid)

instance Arbitrary Name where
    arbitrary = do
        ch <- choose ('a', 'z')
        return $ Name $ T.pack [ch]
    shrink (Name n) = [ Name (T.drop 1 n) | T.length n > 1 ]


data Expression
    = Constant Constant
    | AbstractLiteral (AbstractLiteral Expression)
    | Domain (Domain () Expression)
    | Reference Name
    | WithLocals Expression [Statement]
    | Op Name [Expression]
    | Lambda AbstractPattern Expression
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Expression
instance Hashable Expression
instance ToJSON Expression where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON Expression where parseJSON = JSON.genericParseJSON jsonOptions


-- TODO: Add support for AbsPatTuple
-- TODO: Add support for AbsPatMatrix
-- TODO: Add support for AbsPatSet
lambdaToFunction :: AbstractPattern -> Expression -> (AbstractPattern -> Expression)
lambdaToFunction (Single nm _) body =
    let
        replacer nm2 (Reference n) | n == nm = Reference nm2
        replacer _ x = x

        newBody (Single nm2 _) = transform (replacer nm2) body
        newBody p = bug $ "Incompatible AbstractPattern, expecting `Single` but got " <+> pretty (show p)
    in
        newBody
lambdaToFunction p _ = bug $ "Unsupported AbstractPattern, expecting `Single` but got " <+> pretty (show p)


data DomainDefn
    = DDEnum DomainDefnEnum
    | DDUnnamed DomainDefnUnnamed
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize DomainDefn
instance Hashable DomainDefn
instance ToJSON DomainDefn where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON DomainDefn where parseJSON = JSON.genericParseJSON jsonOptions


data DomainDefnEnum = DomainDefnEnum Name [Name]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize DomainDefnEnum
instance Hashable DomainDefnEnum
instance ToJSON DomainDefnEnum where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON DomainDefnEnum where parseJSON = JSON.genericParseJSON jsonOptions


data DomainDefnUnnamed = DomainDefnUnnamed Name Expression
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize DomainDefnUnnamed
instance Hashable DomainDefnUnnamed
instance ToJSON DomainDefnUnnamed where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON DomainDefnUnnamed where parseJSON = JSON.genericParseJSON jsonOptions



data Domain r a
    = DomainBool
    | DomainInt [Range a]
    | DomainEnum DomainDefnEnum [Range a]
    | DomainUnnamed DomainDefnUnnamed
    | DomainTuple [Domain r a]
    | DomainMatrix (Domain () a) (Domain r a)
    | DomainSet       r (SetAttr a) (Domain r a)
    | DomainMSet      r (DomainAttributes a) (Domain r a)
    | DomainFunction  r (DomainAttributes a) (Domain r a) (Domain r a)
    | DomainRelation  r (DomainAttributes a) [Domain r a]
    | DomainPartition r (DomainAttributes a) (Domain r a)
    | DomainOp Name [Domain r a]
    | DomainHack a          -- this is an ugly hack to be able to use expressions as domains. will go away later.
    deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

instance (Serialize r, Serialize a) => Serialize (Domain r a)
instance (Hashable r, Hashable a) => Hashable (Domain r a)
instance (ToJSON r, ToJSON a) => ToJSON (Domain r a) where toJSON = JSON.genericToJSON jsonOptions
instance (FromJSON r, FromJSON a) => FromJSON (Domain r a) where parseJSON = JSON.genericParseJSON jsonOptions

instance (Arbitrary r, Arbitrary a) => Arbitrary (Domain r a) where
    arbitrary = sized f
        where
            f 0 = oneof [ return DomainBool
                        , DomainInt <$> arbitrary
                        -- , DomainEnum <$> arbitrary <*> arbitrary
                        ]
            f s = do
                arity <- choose (2 :: Int, 10)
                DomainTuple <$> vectorOf arity (f (div s 10))
    shrink DomainBool = []
    shrink (DomainInt []) = [DomainBool]
    shrink (DomainInt [r]) = DomainBool : DomainInt [] : [DomainInt [r'] | r' <- shrink r]
    shrink (DomainInt rs) = [DomainInt (init rs)]
    shrink _ = []

forgetRepr :: Domain r a -> Domain () a
forgetRepr DomainBool = DomainBool
forgetRepr (DomainInt rs) = DomainInt rs
forgetRepr (DomainEnum defn rs) = DomainEnum defn rs
forgetRepr (DomainUnnamed defn) = DomainUnnamed defn
forgetRepr (DomainTuple ds) = DomainTuple (map forgetRepr ds)
forgetRepr (DomainMatrix index inner) = DomainMatrix index (forgetRepr inner)
forgetRepr (DomainSet       _ attr d) = DomainSet () attr (forgetRepr d)
forgetRepr (DomainMSet      _ attr d) = DomainMSet () attr (forgetRepr d)
forgetRepr (DomainFunction  _ attr d1 d2) = DomainFunction () attr (forgetRepr d1) (forgetRepr d2)
forgetRepr (DomainRelation  _ attr ds) = DomainRelation () attr (map forgetRepr ds)
forgetRepr (DomainPartition _ attr d) = DomainPartition () attr (forgetRepr d)
forgetRepr (DomainOp op ds) = DomainOp op (map forgetRepr ds)
forgetRepr (DomainHack a) = DomainHack a


data SetAttr a
    = SetAttrNone
    | SetAttrSize a
    | SetAttrMinSize a
    | SetAttrMaxSize a
    | SetAttrMinMaxSize a a
    deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

instance Serialize a => Serialize (SetAttr a)
instance Hashable a => Hashable (SetAttr a)
instance ToJSON a => ToJSON (SetAttr a) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON a => FromJSON (SetAttr a) where parseJSON = JSON.genericParseJSON jsonOptions


instance Default (SetAttr a) where
    def = SetAttrNone


data DomainAttributes a = DomainAttributes [DomainAttribute a]
    deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

instance Serialize a => Serialize (DomainAttributes a)
instance Hashable a => Hashable (DomainAttributes a)
instance ToJSON a => ToJSON (DomainAttributes a) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON a => FromJSON (DomainAttributes a) where parseJSON = JSON.genericParseJSON jsonOptions

instance Default (DomainAttributes a) where
    def = DomainAttributes []


data DomainAttribute a
    = DAName Name
    | DANameValue Name a
    | DADotDot
    deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

instance Serialize a => Serialize (DomainAttribute a)
instance Hashable a => Hashable (DomainAttribute a)
instance ToJSON a => ToJSON (DomainAttribute a) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON a => FromJSON (DomainAttribute a) where parseJSON = JSON.genericParseJSON jsonOptions


data Range a
    = RangeOpen
    | RangeSingle a
    | RangeLowerBounded a
    | RangeUpperBounded a
    | RangeBounded a a
    deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

instance Serialize a => Serialize (Range a)
instance Hashable a => Hashable (Range a)
instance ToJSON a => ToJSON (Range a) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON a => FromJSON (Range a) where parseJSON = JSON.genericParseJSON jsonOptions

instance Arbitrary a => Arbitrary (Range a) where
    arbitrary = oneof
        [ return RangeOpen
        , RangeSingle <$> arbitrary
        , RangeLowerBounded <$> arbitrary
        , RangeUpperBounded <$> arbitrary
        , RangeBounded <$> arbitrary <*> arbitrary
        ]

rangesInts :: MonadError Doc m => [Range Constant] -> m [Int]
rangesInts = liftM (sortNub . concat) . mapM rangeInts
    where
        rangeInts :: MonadError Doc m => Range Constant -> m [Int]
        rangeInts (RangeSingle (ConstantInt x)) = return [x]
        rangeInts (RangeBounded (ConstantInt x) (ConstantInt y)) = return [x .. y]
        rangeInts _ = throwError "Infinite range (or not an integer range)"


data HasRepresentation = NoRepresentation | HasRepresentation Name
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize HasRepresentation
instance Hashable HasRepresentation
instance ToJSON HasRepresentation where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON HasRepresentation where parseJSON = JSON.genericParseJSON jsonOptions

instance IsString HasRepresentation where
    fromString = HasRepresentation . Name . T.pack


data Type
    = TypeAny
    | TypeBool
    | TypeInt
    | TypeEnum DomainDefnEnum
    | TypeUnnamed DomainDefnUnnamed
    | TypeTuple [Type]
    | TypeMatrix Type Type
    | TypeSet       Type
    | TypeMSet      Type
    | TypeFunction  Type Type
    | TypeRelation  [Type]
    | TypePartition Type
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Type
instance Hashable Type
instance ToJSON Type where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON Type where parseJSON = JSON.genericParseJSON jsonOptions


data Constant
    = ConstantBool Bool
    | ConstantInt Int
    | ConstantEnum DomainDefnEnum Name
    | ConstantTuple [Constant]
    | ConstantMatrix (Domain () Constant) [Constant]
    | ConstantSet [Constant]
    | ConstantMSet [Constant]
    | ConstantFunction [(Constant, Constant)]
    | ConstantRelation [[Constant]]
    | ConstantPartition [[Constant]]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Constant
instance Hashable Constant
instance ToJSON Constant where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON Constant where parseJSON = JSON.genericParseJSON jsonOptions

instance Arbitrary Constant where
    arbitrary = oneof
        [ ConstantBool <$> arbitrary
        , ConstantInt <$> arbitrary
        ]


data AbstractLiteral a
    = AbsLitTuple [a]
    | AbsLitMatrix (Domain () a) [a]
    | AbsLitSet [a]
    | AbsLitMSet [a]
    | AbsLitFunction [(a, a)]
    | AbsLitRelation [[a]]
    | AbsLitPartition [[a]]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize a => Serialize (AbstractLiteral a)
instance Hashable a => Hashable (AbstractLiteral a)
instance ToJSON a => ToJSON (AbstractLiteral a) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON a => FromJSON (AbstractLiteral a) where parseJSON = JSON.genericParseJSON jsonOptions


data AbstractPattern
    = Single Name Type
    | AbsPatTuple [AbstractPattern]
    | AbsPatMatrix
            -- (Domain () a)          -- TODO: Should there be a domain here?
            [AbstractPattern]
    | AbsPatSet [AbstractPattern]
    -- | AbsPatMSet [a]
    -- | AbsPatFunction [(a, a)]
    -- | AbsPatRelation [[a]]
    -- | AbsPatPartition [[a]]
    -- TODO: Consider introducing the above as abstract patterns...
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize AbstractPattern
instance Hashable AbstractPattern
instance ToJSON AbstractPattern where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON AbstractPattern where parseJSON = JSON.genericParseJSON jsonOptions


class ExpressionLike a where
    fromInt :: Int -> a

instance ExpressionLike Constant where
    fromInt = ConstantInt

instance ExpressionLike Expression where
    fromInt = Constant . fromInt


instance Num Expression where
    a + b = Op "+" [a,b]
    a - b = Op "-" [a,b]
    a * b = Op "*" [a,b]
    abs a = Op "abs" [a]
    signum _ = bug "signum {Expression}"
    fromInteger = fromInt . fromInteger

instance Integral Expression where
    divMod a b = (Op "/" [a,b], Op "%" [a,b])
    quotRem = divMod
    toInteger = bug "toInteger {Expression}"

instance Real Expression where
    toRational = bug "toRational {Expression}"

instance Enum Expression where
    fromEnum = bug "fromEnum {Expression}"
    toEnum = fromInt
    succ a = a + 1
    pred a = a - 1
    enumFrom x = x : enumFrom (succ x)
    enumFromThen x n = x : enumFromThen (x+n) n
    enumFromTo _x _y = bug "enumFromTo {Expression}"
    enumFromThenTo _x _n _y = bug "enumFromThenTo {Expression}"


