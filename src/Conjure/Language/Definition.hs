{-# LANGUAGE DeriveGeneric, DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.Language.Definition
    ( forgetRepr, rangesInts

    , Model(..), LanguageVersion(..)
    , ModelInfo(..), Decision(..)
    , Statement(..), Objective(..), Declaration(..)

    , Name(..)
    , Expression(..)
    , Constant(..)

    , Domain(..), Range(..)
    , DomainDefnEnum(..), DomainDefnUnnamed(..)

    , SetAttr(..)
    , DomainAttributes(..), DomainAttribute(..)

    , HasRepresentation(..)

    , Type(..)

    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug

-- base
import GHC.Generics ( Generic )
import GHC.Show ( showSpace, showList__ )

-- text
import qualified Data.Text as T

-- aeson
import Data.Aeson ( ToJSON(..), (.=) )
import qualified Data.Aeson as JSON

-- cereal
import Data.Serialize as Serialize ( Serialize(..) )

-- QuickCheck
import Test.QuickCheck ( Arbitrary(..), choose, oneof, vectorOf, sized )


data Model = Model
    { mLanguage :: LanguageVersion
    , mStatements :: [Statement]
    , mInfo :: ModelInfo
    }
    deriving (Eq, Ord, Show, Generic)

instance Serialize Model
instance Hashable Model
instance ToJSON Model

instance Default Model where
    def = Model def [] def


data LanguageVersion = LanguageVersion Name [Int]
    deriving (Eq, Ord, Show, Generic)

instance Serialize LanguageVersion
instance Hashable LanguageVersion

instance ToJSON LanguageVersion where
    toJSON (LanguageVersion t is) = JSON.object [ "language" .= toJSON (t,is) ]

instance Default LanguageVersion where
    def = LanguageVersion "Essence" [2,0]


data Statement
    = Declaration Declaration
    | Where Expression
    | Objective Objective Expression
    | SuchThat Expression
    deriving (Eq, Ord, Show, Generic)

instance Serialize Statement
instance Hashable Statement
instance ToJSON Statement


data Objective = Minimising | Maximising
    deriving (Eq, Ord, Show, Generic)

instance Serialize Objective
instance Hashable Objective
instance ToJSON Objective


data Declaration
    = Find    Name (Domain () Expression)
    | Given   Name (Domain () Expression)
    | Letting Name Expression
    deriving (Eq, Ord, Show, Generic)

instance Serialize Declaration
instance Hashable Declaration
instance ToJSON Declaration


data ModelInfo = ModelInfo
    { miRepresentations :: [(Name, Domain HasRepresentation Expression)]
    , miTrail :: [Decision]
    }
    deriving (Eq, Ord, Show, Generic)

instance Serialize ModelInfo
instance Hashable ModelInfo
instance ToJSON ModelInfo

instance Default ModelInfo where
    def = ModelInfo [] []


data Decision = Decision
    { dDescription :: Text
    , dOptions :: [Int]
    , dDecision :: Int
    }
    deriving (Eq, Ord, Show, Generic)

instance Serialize Decision
instance Hashable Decision
instance ToJSON Decision


newtype Name = Name Text
    deriving (Eq, Ord, Show, Generic, IsString, Serialize, Hashable, ToJSON, Monoid)

instance Arbitrary Name where
    arbitrary = do
        ch <- choose ('a', 'z')
        return $ Name $ T.pack [ch]
    shrink (Name n) = if T.length n > 1 then [Name (T.drop 1 n)] else []


data Expression
    = Constant Constant
    | Reference Name
    | Op Name [Expression]
    | Lambda Name Type Expression (Expression -> Expression)
    deriving (Generic)

instance Eq Expression where
    Constant a == Constant b = a == b
    Reference a == Reference b = a == b
    Op n1 xs1 == Op n2 xs2 = n1 == n2 && and (zipWith (==) xs1 xs2)
    Lambda {} == Lambda {} = bug "Lambda's cannot be compared for equality. There you go."
    _ == _ = False

instance Ord Expression where
    Constant  a  `compare` Constant b  = a `compare` b
    Constant  {} `compare` _ = LT
    Reference a  `compare` Reference b = a `compare` b
    Reference {} `compare` _ = LT
    Op nm1 xs1   `compare` Op nm2 xs2 =
        case nm1 `compare` nm2 of
            EQ -> xs1 `compare` xs2
            ow -> ow
    Op {} `compare` _ = LT
    Lambda {} `compare` _ = bug "Lambda's cannot be compared for ordering. There you go."

instance Show Expression where
    showsPrec pr (Constant x       ) = showParen (pr >= 11) (showString "Constant "  . showsPrec 11 x)
    showsPrec pr (Reference x      ) = showParen (pr >= 11) (showString "Reference " . showsPrec 11 x)
    showsPrec pr (Op op xs         ) = showParen (pr >= 11) (showString "Op "        . showsPrec 11 op
                                                                         . showSpace . showsPrec 11 xs)
    showsPrec pr (Lambda arg ty x _) = showParen (pr >= 11) (showString "Lambda "    . showsPrec 11 arg
                                                                         . showSpace . showsPrec 11 ty
                                                                         . showSpace . showsPrec 11 x)
    showList = showList__ (showsPrec 0)

instance Serialize Expression where
    put (Constant x       ) = Serialize.put (0 :: Int) >> Serialize.put x
    put (Reference x      ) = Serialize.put (1 :: Int) >> Serialize.put x
    put (Op op xs         ) = Serialize.put (2 :: Int) >> Serialize.put op  >> Serialize.put xs
    put (Lambda arg ty x _) = Serialize.put (3 :: Int) >> Serialize.put arg >> Serialize.put ty >> Serialize.put x
    get = do
        tag <- Serialize.get
        case tag :: Int of
            0 -> Constant <$> Serialize.get
            1 -> Reference <$> Serialize.get
            2 -> Op <$> Serialize.get <*> Serialize.get
            3 -> Lambda <$> Serialize.get <*> Serialize.get <*> Serialize.get <*> error "Serialize.get for Lambda"
            _ -> bug "While deserialising an expression"

instance Hashable Expression where
    hashWithSalt salt (Constant x       ) = hashWithSalt salt x
    hashWithSalt salt (Reference x      ) = hashWithSalt salt x
    hashWithSalt salt (Op op xs         ) = hashWithSalt salt (op,xs)
    hashWithSalt salt (Lambda arg ty x _) = hashWithSalt salt (arg,ty,x)

instance ToJSON Expression where
    toJSON (Constant x       ) = JSON.object [ "Constant"       .= toJSON x  ]
    toJSON (Reference x      ) = JSON.object [ "Reference"      .= toJSON x  ]
    toJSON (Op op xs         ) = JSON.object [ "Op-Name"        .= toJSON op
                                             , "Op-Args"        .= toJSON xs ]
    toJSON (Lambda arg ty x _) = JSON.object [ "Lambda-Arg"     .= toJSON arg
                                             , "Lambda-ArgType" .= toJSON ty
                                             , "Lambda-Body"    .= toJSON x  ]


data DomainDefnEnum = DomainDefnEnum Name [Name]
    deriving (Eq, Ord, Show, Generic)

instance Serialize DomainDefnEnum
instance Hashable DomainDefnEnum
instance ToJSON DomainDefnEnum


data DomainDefnUnnamed = DomainDefnUnnamed Name Expression
    deriving (Eq, Ord, Show, Generic)

instance Serialize DomainDefnUnnamed
instance Hashable DomainDefnUnnamed
instance ToJSON DomainDefnUnnamed


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
    deriving (Eq, Ord, Show, Generic, Functor)

instance (Serialize r, Serialize a) => Serialize (Domain r a)
instance (Hashable r, Hashable a) => Hashable (Domain r a)
instance (ToJSON r, ToJSON a) => ToJSON (Domain r a)

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
    | SetAttrDotDot (SetAttr a)
    deriving (Eq, Ord, Show, Generic, Functor)

instance Serialize a => Serialize (SetAttr a)
instance Hashable a => Hashable (SetAttr a)
instance ToJSON a => ToJSON (SetAttr a)

instance Default (SetAttr a) where
    def = SetAttrNone


data DomainAttributes a = DomainAttributes [DomainAttribute a]
    deriving (Eq, Ord, Show, Generic, Functor)

instance Serialize a => Serialize (DomainAttributes a)
instance Hashable a => Hashable (DomainAttributes a)
instance ToJSON a => ToJSON (DomainAttributes a)

instance Default (DomainAttributes a) where
    def = DomainAttributes []


data DomainAttribute a
    = DAName Name
    | DANameValue Name a
    | DADotDot
    deriving (Eq, Ord, Show, Generic, Functor)

instance Serialize a => Serialize (DomainAttribute a)
instance Hashable a => Hashable (DomainAttribute a)
instance ToJSON a => ToJSON (DomainAttribute a)


data Range a
    = RangeOpen
    | RangeSingle a
    | RangeLowerBounded a
    | RangeUpperBounded a
    | RangeBounded a a
    deriving (Eq, Ord, Show, Generic, Functor)

instance Serialize a => Serialize (Range a)
instance Hashable a => Hashable (Range a)
instance ToJSON a => ToJSON (Range a)

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
    deriving (Eq, Ord, Show, Generic)

instance Serialize HasRepresentation
instance Hashable HasRepresentation
instance ToJSON HasRepresentation

instance IsString HasRepresentation where
    fromString = HasRepresentation . Name . T.pack


data Type
    = TypeBool
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
    deriving (Eq, Ord, Show, Generic)

instance Serialize Type
instance Hashable Type
instance ToJSON Type


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
    deriving (Eq, Ord, Show, Generic)

instance Serialize Constant
instance Hashable Constant
instance ToJSON Constant

instance Arbitrary Constant where
    arbitrary = oneof
        [ ConstantBool <$> arbitrary
        , ConstantInt <$> arbitrary
        ]

