{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Conjure.Language.Domain where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Name
import Conjure.Language.Type
import Conjure.Language.TypeOf
import Conjure.Language.AdHoc
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
    | DomainEnum Name (Maybe [Range Name]) -- subset of values for this domain
                                           -- Nothing *only* when GivenDomainDefnEnum and not LettingDomainDefnEnum
    | DomainUnnamed Name x
    | DomainTuple [Domain r x]
    | DomainMatrix (Domain () x) (Domain r x)
    | DomainSet       r (SetAttr x) (Domain r x)
    | DomainMSet      r (DomainAttributes x) (Domain r x)
    | DomainFunction  r (FunctionAttr x) (Domain r x) (Domain r x)
    | DomainRelation  r (RelationAttr x) [Domain r x]
    | DomainPartition r (DomainAttributes x) (Domain r x)
    | DomainOp Name [Domain r x]
    | DomainReference Name (Maybe (Domain r x))
    | DomainMetaVar String
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

instance TypeOf (Domain r x) where
    typeOf DomainBool                = return TypeBool
    typeOf DomainInt{}               = return TypeInt
    typeOf (DomainEnum    defn _   ) = return (TypeEnum defn)
    typeOf (DomainUnnamed defn _   ) = return (TypeUnnamed defn)
    typeOf (DomainTuple         xs ) = TypeTuple      <$> mapM typeOf xs
    typeOf (DomainMatrix ind inn   ) = TypeMatrix     <$> typeOf ind <*> typeOf inn
    typeOf (DomainSet       _ _ x  ) = TypeSet        <$> typeOf x
    typeOf (DomainMSet      _ _ x  ) = TypeMSet       <$> typeOf x
    typeOf (DomainFunction  _ _ x y) = TypeFunction   <$> typeOf x <*> typeOf y
    typeOf (DomainRelation  _ _ xs ) = TypeRelation   <$> mapM typeOf xs
    typeOf (DomainPartition _ _ x  ) = TypePartition  <$> typeOf x
    typeOf DomainOp{} = bug "typeOf DomainOp"
    typeOf (DomainReference _ (Just d)) = typeOf d
    typeOf (DomainReference nm Nothing) = bug $ "typeOf: DomainReference" <+> pretty nm
    typeOf (DomainMetaVar nm) = bug $ "typeOf: DomainMetaVar &" <> pretty nm

forgetRepr :: Domain r x -> Domain () x
forgetRepr DomainBool = DomainBool
forgetRepr (DomainInt rs) = DomainInt rs
forgetRepr (DomainEnum defn rs) = DomainEnum defn rs
forgetRepr (DomainUnnamed defn s) = DomainUnnamed defn s
forgetRepr (DomainTuple ds) = DomainTuple (map forgetRepr ds)
forgetRepr (DomainMatrix index inner) = DomainMatrix index (forgetRepr inner)
forgetRepr (DomainSet       _ attr d) = DomainSet () attr (forgetRepr d)
forgetRepr (DomainMSet      _ attr d) = DomainMSet () attr (forgetRepr d)
forgetRepr (DomainFunction  _ attr d1 d2) = DomainFunction () attr (forgetRepr d1) (forgetRepr d2)
forgetRepr (DomainRelation  _ attr ds) = DomainRelation () attr (map forgetRepr ds)
forgetRepr (DomainPartition _ attr d) = DomainPartition () attr (forgetRepr d)
forgetRepr (DomainOp op ds) = DomainOp op (map forgetRepr ds)
forgetRepr (DomainReference x r) = DomainReference x (fmap forgetRepr r)
forgetRepr (DomainMetaVar x) = DomainMetaVar x

reprAtTopLevel :: Domain r x -> Maybe r
reprAtTopLevel DomainBool{} = Nothing
reprAtTopLevel DomainInt{} = Nothing
reprAtTopLevel DomainEnum{} = Nothing
reprAtTopLevel DomainUnnamed{} = Nothing
reprAtTopLevel DomainTuple{} = Nothing
reprAtTopLevel DomainMatrix{} = Nothing
reprAtTopLevel (DomainSet       r _ _  ) = return r
reprAtTopLevel (DomainMSet      r _ _  ) = return r
reprAtTopLevel (DomainFunction  r _ _ _) = return r
reprAtTopLevel (DomainRelation  r _ _  ) = return r
reprAtTopLevel (DomainPartition r _ _  ) = return r
reprAtTopLevel DomainOp{} = Nothing
reprAtTopLevel DomainReference{} = Nothing
reprAtTopLevel DomainMetaVar{} = Nothing

isPrimitiveDomain :: Domain r x -> Bool
isPrimitiveDomain DomainBool{} = True
isPrimitiveDomain DomainInt{} = True
isPrimitiveDomain (DomainMatrix index inner) = and [isPrimitiveDomain index, isPrimitiveDomain inner]
isPrimitiveDomain _ = False

getIndices :: Domain r x -> ([Domain () x], Domain r x)
getIndices (DomainMatrix index inner) = first (index:) (getIndices inner)
getIndices d = ([], d)

domainCanIndexMatrix :: Domain r x -> Bool
domainCanIndexMatrix DomainBool{} = True
domainCanIndexMatrix DomainInt {} = True
domainCanIndexMatrix _            = False


--------------------------------------------------------------------------------
-- attribute definitions -------------------------------------------------------
--------------------------------------------------------------------------------

data SetAttr a = SetAttr (SizeAttr a)
    deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)
instance Serialize a => Serialize (SetAttr a)
instance Hashable  a => Hashable  (SetAttr a)
instance ToJSON    a => ToJSON    (SetAttr a) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (SetAttr a) where parseJSON = JSON.genericParseJSON jsonOptions
instance Default (SetAttr a) where def = SetAttr def
instance Pretty a => Pretty (SetAttr a) where
    pretty (SetAttr SizeAttrNone) = prEmpty
    pretty (SetAttr a) = prParens (pretty a)


data SizeAttr a
    = SizeAttrNone
    | SizeAttrSize a
    | SizeAttrMinSize a
    | SizeAttrMaxSize a
    | SizeAttrMinMaxSize a a
    deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)
instance Serialize a => Serialize (SizeAttr a)
instance Hashable  a => Hashable  (SizeAttr a)
instance ToJSON    a => ToJSON    (SizeAttr a) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (SizeAttr a) where parseJSON = JSON.genericParseJSON jsonOptions
instance Default (SizeAttr a) where def = SizeAttrNone
instance Pretty a => Pretty (SizeAttr a) where
    pretty SizeAttrNone = prEmpty
    pretty (SizeAttrSize       a  ) = "size"    <+> pretty a
    pretty (SizeAttrMinSize    a  ) = "minSize" <+> pretty a
    pretty (SizeAttrMaxSize    a  ) = "maxSize" <+> pretty a
    pretty (SizeAttrMinMaxSize a b) = "minSize" <+> pretty a <+> ", maxSize" <+> pretty b


data FunctionAttr x
    = FunctionAttr (SizeAttr x) PartialityAttr ISBAttr
    deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)
instance Serialize a => Serialize (FunctionAttr a)
instance Hashable  a => Hashable  (FunctionAttr a)
instance ToJSON    a => ToJSON    (FunctionAttr a) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (FunctionAttr a) where parseJSON = JSON.genericParseJSON jsonOptions
instance Default (FunctionAttr a) where def = FunctionAttr def def def
instance Pretty a => Pretty (FunctionAttr a) where
    pretty (FunctionAttr a b c) =
        let inside = filter (/=prEmpty) [pretty a, pretty b, pretty c]
        in  if null inside
                then prEmpty
                else prettyList prParens "," inside


data PartialityAttr
    = FunctionAttr_Partial
    | FunctionAttr_Total
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize PartialityAttr
instance Hashable  PartialityAttr
instance ToJSON    PartialityAttr where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  PartialityAttr where parseJSON = JSON.genericParseJSON jsonOptions
instance Default   PartialityAttr where def = FunctionAttr_Partial
instance Pretty    PartialityAttr where
    pretty FunctionAttr_Partial = prEmpty -- partial is the default
    pretty FunctionAttr_Total = "total"


data ISBAttr
    = ISBAttr_None
    | ISBAttr_Injective
    | ISBAttr_Surjective
    | ISBAttr_Bijective
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize ISBAttr
instance Hashable  ISBAttr
instance ToJSON    ISBAttr where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  ISBAttr where parseJSON = JSON.genericParseJSON jsonOptions
instance Default   ISBAttr where def = ISBAttr_None
instance Pretty    ISBAttr where
    pretty ISBAttr_None = prEmpty
    pretty ISBAttr_Injective = "injective"
    pretty ISBAttr_Surjective = "surjective"
    pretty ISBAttr_Bijective = "bijective"


data RelationAttr a = RelationAttr (SizeAttr a)
    deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)
instance Serialize a => Serialize (RelationAttr a)
instance Hashable  a => Hashable  (RelationAttr a)
instance ToJSON    a => ToJSON    (RelationAttr a) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (RelationAttr a) where parseJSON = JSON.genericParseJSON jsonOptions
instance Default (RelationAttr a) where def = RelationAttr def
instance Pretty a => Pretty (RelationAttr a) where
    pretty (RelationAttr SizeAttrNone) = prEmpty
    pretty (RelationAttr a) = prParens (pretty a)


data DomainAttributes a = DomainAttributes [DomainAttribute a]
    deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

instance Serialize a => Serialize (DomainAttributes a)
instance Hashable  a => Hashable  (DomainAttributes a)
instance ToJSON    a => ToJSON    (DomainAttributes a) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (DomainAttributes a) where parseJSON = JSON.genericParseJSON jsonOptions

instance Default (DomainAttributes a) where
    def = DomainAttributes []


data DomainAttribute a
    = DAName Name
    | DANameValue Name a
    | DADotDot
    deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

instance Serialize a => Serialize (DomainAttribute a)
instance Hashable  a => Hashable  (DomainAttribute a)
instance ToJSON    a => ToJSON    (DomainAttribute a) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (DomainAttribute a) where parseJSON = JSON.genericParseJSON jsonOptions


data Range a
    = RangeOpen
    | RangeSingle a
    | RangeLowerBounded a
    | RangeUpperBounded a
    | RangeBounded a a
    deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor)

instance Serialize a => Serialize (Range a)
instance Hashable  a => Hashable (Range a)
instance ToJSON    a => ToJSON (Range a) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  a => FromJSON (Range a) where parseJSON = JSON.genericParseJSON jsonOptions

instance Arbitrary a => Arbitrary (Range a) where
    arbitrary = oneof
        [ return RangeOpen
        , RangeSingle <$> arbitrary
        , RangeLowerBounded <$> arbitrary
        , RangeUpperBounded <$> arbitrary
        , RangeBounded <$> arbitrary <*> arbitrary
        ]

rangesInts :: (MonadFail m, ExpressionLike c) => [Range c] -> m [Int]
rangesInts = liftM (sortNub . concat) . mapM rangeInts
    where
        rangeInts (RangeSingle x) = return <$> intOut x
        rangeInts (RangeBounded x y) = do x' <- intOut x
                                          y' <- intOut y
                                          return [x' .. y']
        rangeInts _ = fail "Infinite range (or not an integer range)"


data HasRepresentation = NoRepresentation | HasRepresentation Name
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize HasRepresentation
instance Hashable  HasRepresentation
instance ToJSON    HasRepresentation where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  HasRepresentation where parseJSON = JSON.genericParseJSON jsonOptions

instance IsString  HasRepresentation where
    fromString = HasRepresentation . Name . T.pack





instance (Pretty r, Pretty a) => Pretty (Domain r a) where
    -- domain.*

    pretty DomainBool = "bool"

    pretty (DomainInt []) = "int"
    pretty (DomainInt ranges) = "int" <> prettyList prParens "," ranges

    pretty (DomainEnum name (Just ranges)) = pretty name <> prettyList prParens "," ranges
    pretty (DomainEnum name _) = pretty name

    pretty (DomainUnnamed name _) = pretty name

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

    pretty (DomainReference x _) = pretty x

    pretty (DomainMetaVar x) = "&" <> pretty x


prettyAttrs :: (Pretty a, Pretty b) => a -> b -> Doc
prettyAttrs a bs =
    let prettya = pretty a
    in  if prettya == "()"
            then pretty bs
            else prBraces prettya <+> pretty bs

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
