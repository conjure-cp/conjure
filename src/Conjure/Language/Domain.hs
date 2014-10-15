{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Conjure.Language.Domain where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Name
import Conjure.Language.DomainDefn
import Conjure.Language.Type
import Conjure.Language.TypeCheck
import Conjure.Language.IntContainer
import Conjure.Language.Pretty

-- text
import qualified Data.Text as T

-- aeson
import qualified Data.Aeson as JSON

-- QuickCheck
import Test.QuickCheck ( Arbitrary(..), choose, oneof, vectorOf, sized )


data Domain r x
    = DomainBool
    | DomainInt [Range x]
    | DomainEnum DomainDefnEnum [Range x]
    | DomainUnnamed DomainDefnUnnamed
    | DomainTuple [Domain r x]
    | DomainMatrix (Domain () x) (Domain r x)
    | DomainSet       r (SetAttr x) (Domain r x)
    | DomainMSet      r (DomainAttributes x) (Domain r x)
    | DomainFunction  r (DomainAttributes x) (Domain r x) (Domain r x)
    | DomainRelation  r (DomainAttributes x) [Domain r x]
    | DomainPartition r (DomainAttributes x) (Domain r x)
    | DomainOp Name [Domain r x]
    | DomainHack x
    deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

instance (Serialize r, Serialize x) => Serialize (Domain r x)
instance (Hashable  r, Hashable  x) => Hashable  (Domain r x)
instance (ToJSON    r, ToJSON    x) => ToJSON    (Domain r x) where toJSON = JSON.genericToJSON jsonOptions
instance (FromJSON  r, FromJSON  x) => FromJSON  (Domain r x) where parseJSON = JSON.genericParseJSON jsonOptions

instance (Arbitrary r, Arbitrary x) => Arbitrary (Domain r x) where
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

instance TypeOf st (Domain r x) where
    typeOf DomainBool                = return TypeBool
    typeOf DomainInt{}               = return TypeInt
    typeOf (DomainEnum    defn _   ) = return (TypeEnum defn)
    typeOf (DomainUnnamed defn     ) = return (TypeUnnamed defn)
    typeOf (DomainTuple         xs ) = TypeTuple      <$> mapM typeOf xs
    typeOf (DomainMatrix ind inn   ) = TypeMatrix     <$> typeOf ind <*> typeOf inn
    typeOf (DomainSet       _ _ x  ) = TypeSet        <$> typeOf x
    typeOf (DomainMSet      _ _ x  ) = TypeMSet       <$> typeOf x
    typeOf (DomainFunction  _ _ x y) = TypeFunction   <$> typeOf x <*> typeOf y
    typeOf (DomainRelation  _ _ xs ) = TypeRelation   <$> mapM typeOf xs
    typeOf (DomainPartition _ _ x  ) = TypePartition  <$> typeOf x
    typeOf DomainOp{} = bug "typeOf DomainOp"
    typeOf DomainHack{} = bug "typeOf DomainHack"

forgetRepr :: Domain r x -> Domain () x
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
forgetRepr (DomainHack x) = DomainHack x

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

rangesInts :: (MonadFail m, IntContainer c) => [Range c] -> m [Int]
rangesInts = liftM (sortNub . concat) . mapM rangeInts
    where
        rangeInts (RangeSingle x) = return [intOut x]
        rangeInts (RangeBounded x y) = return [intOut x .. intOut y]
        rangeInts _ = fail "Infinite range (or not an integer range)"


data HasRepresentation = NoRepresentation | HasRepresentation Name
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize HasRepresentation
instance Hashable HasRepresentation
instance ToJSON HasRepresentation where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON HasRepresentation where parseJSON = JSON.genericParseJSON jsonOptions

instance IsString HasRepresentation where
    fromString = HasRepresentation . Name . T.pack





instance (Pretty r, Pretty a) => Pretty (Domain r a) where
    -- domain.*

    pretty DomainBool = "bool"

    pretty (DomainInt []) = "int"
    pretty (DomainInt ranges) = "int" <> prettyList prParens "," ranges

    pretty (DomainEnum (DomainDefnEnum name _) []) = pretty name
    pretty (DomainEnum (DomainDefnEnum name _) ranges) = pretty name <> prettyList prParens "," ranges

    pretty (DomainUnnamed (DomainDefnUnnamed name)) = pretty name

    pretty (DomainTuple inners)
        = (if length inners < 2 then "tuple" else prEmpty)
        <+> prettyList prParens "," inners

    pretty (DomainMatrix index innerNested)
        = "matrix indexed by" <+> prettyList prBrackets "," indices
                              <+> "of" <+> pretty inner
        where
            (indices,inner) = first (index:) $ collect innerNested
            collect (DomainMatrix i j) = first (i:) $ collect j
            collect x = ([],x)

    pretty (DomainSet r attrs inner) =
        hang ("set" <+> prettyAttrs r attrs <+> "of") 4 (pretty inner)

    pretty (DomainMSet r attrs inner) =
        hang ("mset" <+> prettyAttrs r attrs <+> "of") 4 (pretty inner)

    pretty (DomainFunction r attrs innerFrom innerTo) =
        hang ("function" <+> prettyAttrs r attrs) 4 $
            hang (pretty innerFrom) 4 $
                "-->" <+> pretty innerTo

    pretty (DomainRelation r attrs inners)
        = hang ("relation" <+> prettyAttrs r attrs <+> "of") 4 (prettyList prParens " *" inners)

    pretty (DomainPartition r attrs inner)
        = hang ("partition" <+> prettyAttrs r attrs <+> "from") 4 (pretty inner)

    pretty d@(DomainOp{}) = pretty (show d)

    pretty (DomainHack x) = pretty x


prettyAttrs :: (Pretty a, Pretty b) => a -> b -> Doc
prettyAttrs a bs =
    let prettya = pretty a
    in  if prettya == "()"
            then pretty bs
            else prBraces prettya <+> pretty bs

instance Pretty a => Pretty (SetAttr a) where
    pretty SetAttrNone = prEmpty
    pretty (SetAttrSize       a  ) = prParens ("size"    <+> pretty a)
    pretty (SetAttrMinSize    a  ) = prParens ("minSize" <+> pretty a)
    pretty (SetAttrMaxSize    a  ) = prParens ("maxSize" <+> pretty a)
    pretty (SetAttrMinMaxSize a b) = prParens ("minSize" <+> pretty a <+> ", maxSize" <+> pretty b)

instance Pretty a => Pretty (DomainAttributes a) where
    pretty (DomainAttributes []) = prEmpty
    pretty (DomainAttributes attrs) = prettyList prParens "," attrs

instance Pretty a => Pretty (DomainAttribute a) where
    pretty (DAName name) = pretty name
    pretty (DANameValue name value) = pretty name <+> pretty value
    pretty DADotDot = ".."

instance Pretty a => Pretty (Range a) where
    pretty RangeOpen = ".."
    pretty (RangeSingle x) = pretty x
    pretty (RangeLowerBounded x) = pretty x <> ".."
    pretty (RangeUpperBounded x) = ".." <> pretty x
    pretty (RangeBounded x y) = pretty x <> ".." <> pretty y

instance Pretty HasRepresentation where
    pretty NoRepresentation = "∅"
    pretty (HasRepresentation r) = pretty r
