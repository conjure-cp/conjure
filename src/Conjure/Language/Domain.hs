{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Conjure.Language.Domain
    ( Domain(..)
    , HasRepresentation(..)
    , Range(..), rangesInts
    , SetAttr(..), SizeAttr(..)
    , MSetAttr(..), OccurAttr(..)
    , FunctionAttr(..), PartialityAttr(..), JectivityAttr(..)
    , RelationAttr(..)
    , PartitionAttr(..)
    , DomainAttributes(..), DomainAttribute(..)         -- only for parsing
    , isPrimitiveDomain, domainCanIndexMatrix, getIndices
    , reprAtTopLevel, forgetRepr
    , typeOfDomain
    , normaliseDomain, normaliseRange
    ) where

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
    | DomainEnum
        Name
        (Maybe [Range Name])        -- subset of values for this domain
                                    -- Nothing *only* when GivenDomainDefnEnum and not LettingDomainDefnEnum
        (Maybe [(Name, Int)])       -- the mapping to integers, if available
    | DomainUnnamed Name x
    | DomainTuple [Domain r x]
    | DomainMatrix (Domain () x) (Domain r x)
    | DomainSet       r (SetAttr x) (Domain r x)
    | DomainMSet      r (MSetAttr x) (Domain r x)
    | DomainFunction  r (FunctionAttr x) (Domain r x) (Domain r x)
    | DomainRelation  r (RelationAttr x) [Domain r x]
    | DomainPartition r (PartitionAttr x) (Domain r x)
    | DomainOp Name [Domain r x]
    | DomainReference Name (Maybe (Domain r x))
    | DomainMetaVar String
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

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
    typeOf = typeOfDomain

typeOfDomain :: MonadFail m => Domain r x -> m Type
typeOfDomain DomainBool                = return TypeBool
typeOfDomain DomainInt{}               = return TypeInt
typeOfDomain (DomainEnum    defn _ _ ) = return (TypeEnum defn)
typeOfDomain (DomainUnnamed defn _   ) = return (TypeUnnamed defn)
typeOfDomain (DomainTuple         xs ) = TypeTuple      <$> mapM typeOf xs
typeOfDomain (DomainMatrix ind inn   ) = TypeMatrix     <$> typeOf ind <*> typeOf inn
typeOfDomain (DomainSet       _ _ x  ) = TypeSet        <$> typeOf x
typeOfDomain (DomainMSet      _ _ x  ) = TypeMSet       <$> typeOf x
typeOfDomain (DomainFunction  _ _ x y) = TypeFunction   <$> typeOf x <*> typeOf y
typeOfDomain (DomainRelation  _ _ xs ) = TypeRelation   <$> mapM typeOf xs
typeOfDomain (DomainPartition _ _ x  ) = TypePartition  <$> typeOf x
typeOfDomain DomainOp{} = bug "typeOf DomainOp"
typeOfDomain (DomainReference _ (Just d)) = typeOf d
typeOfDomain (DomainReference nm Nothing) = bug $ "typeOf: DomainReference" <+> pretty nm
typeOfDomain (DomainMetaVar nm) = bug $ "typeOf: DomainMetaVar &" <> pretty nm

instance (Pretty x) => Monoid (Domain () x) where
    mempty = DomainMetaVar "mempty"
    mappend DomainMetaVar{} d = d
    mappend d DomainMetaVar{} = d
    mappend DomainBool DomainBool = DomainBool
    mappend (DomainInt r1) (DomainInt r2) = DomainInt (mappend r1 r2)
    mappend (DomainTuple xs) (DomainTuple ys)
        | length xs == length ys
        = DomainTuple (zipWith mappend xs ys)
    mappend (DomainSet _ _ x) (DomainSet _ _ y)
        = DomainSet () def (mappend x y)
    mappend (DomainMSet _ _ x) (DomainMSet _ _ y)
        = DomainMSet () def (mappend x y)
    mappend (DomainFunction _ _ x1 x2) (DomainFunction _ _ y1 y2)
        = DomainFunction () def (mappend x1 y1) (mappend x2 y2)
    mappend (DomainRelation _ _ xs) (DomainRelation _ _ ys)
        | length xs == length ys
        = DomainRelation () def (zipWith mappend xs ys)
    mappend (DomainPartition _ _ x) (DomainPartition _ _ y)
        = DomainPartition () def (mappend x y)
    mappend d1 d2 = bug $ vcat ["Domain.mappend", pretty d1, pretty d2]

forgetRepr :: Domain r x -> Domain r2 x
forgetRepr DomainBool = DomainBool
forgetRepr (DomainInt rs) = DomainInt rs
forgetRepr (DomainEnum defn rs mp) = DomainEnum defn rs mp
forgetRepr (DomainUnnamed defn s) = DomainUnnamed defn s
forgetRepr (DomainTuple ds) = DomainTuple (map forgetRepr ds)
forgetRepr (DomainMatrix index inner) = DomainMatrix index (forgetRepr inner)
forgetRepr (DomainSet       _ attr d) = DomainSet (error "forgetRepr") attr (forgetRepr d)
forgetRepr (DomainMSet      _ attr d) = DomainMSet (error "forgetRepr") attr (forgetRepr d)
forgetRepr (DomainFunction  _ attr d1 d2) = DomainFunction (error "forgetRepr") attr (forgetRepr d1) (forgetRepr d2)
forgetRepr (DomainRelation  _ attr ds) = DomainRelation (error "forgetRepr") attr (map forgetRepr ds)
forgetRepr (DomainPartition _ attr d) = DomainPartition (error "forgetRepr") attr (forgetRepr d)
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
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize a => Serialize (SetAttr a)
instance Hashable  a => Hashable  (SetAttr a)
instance ToJSON    a => ToJSON    (SetAttr a) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (SetAttr a) where parseJSON = JSON.genericParseJSON jsonOptions
instance Default (SetAttr a) where def = SetAttr def
instance Pretty a => Pretty (SetAttr a) where
    pretty (SetAttr SizeAttr_None) = prEmpty
    pretty (SetAttr a) = prParens (pretty a)


data SizeAttr a
    = SizeAttr_None
    | SizeAttr_Size a
    | SizeAttr_MinSize a
    | SizeAttr_MaxSize a
    | SizeAttr_MinMaxSize a a
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize a => Serialize (SizeAttr a)
instance Hashable  a => Hashable  (SizeAttr a)
instance ToJSON    a => ToJSON    (SizeAttr a) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (SizeAttr a) where parseJSON = JSON.genericParseJSON jsonOptions
instance Default (SizeAttr a) where def = SizeAttr_None
instance Pretty a => Pretty (SizeAttr a) where
    pretty SizeAttr_None = prEmpty
    pretty (SizeAttr_Size       x  ) = "size"    <+> pretty x
    pretty (SizeAttr_MinSize    x  ) = "minSize" <+> pretty x
    pretty (SizeAttr_MaxSize    x  ) = "maxSize" <+> pretty x
    pretty (SizeAttr_MinMaxSize x y) = "minSize" <+> pretty x <+> ", maxSize" <+> pretty y


data MSetAttr a = MSetAttr (SizeAttr a) (OccurAttr a)
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize a => Serialize (MSetAttr a)
instance Hashable  a => Hashable  (MSetAttr a)
instance ToJSON    a => ToJSON    (MSetAttr a) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (MSetAttr a) where parseJSON = JSON.genericParseJSON jsonOptions
instance Default (MSetAttr a) where def = MSetAttr def def
instance Pretty a => Pretty (MSetAttr a) where
    pretty (MSetAttr a b) = 
        let inside = filter (/=prEmpty) [ pretty a
                                        , pretty b
                                        ]
        in  if null inside
                then prEmpty
                else prettyList prParens "," inside


data OccurAttr a
    = OccurAttr_None
    | OccurAttr_MinOccur a
    | OccurAttr_MaxOccur a
    | OccurAttr_MinMaxOccur a a
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize a => Serialize (OccurAttr a)
instance Hashable  a => Hashable  (OccurAttr a)
instance ToJSON    a => ToJSON    (OccurAttr a) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (OccurAttr a) where parseJSON = JSON.genericParseJSON jsonOptions
instance Default (OccurAttr a) where def = OccurAttr_None
instance Pretty a => Pretty (OccurAttr a) where
    pretty OccurAttr_None = prEmpty
    pretty (OccurAttr_MinOccur    x  ) = "minOccur" <+> pretty x
    pretty (OccurAttr_MaxOccur    x  ) = "maxOccur" <+> pretty x
    pretty (OccurAttr_MinMaxOccur x y) = "minOccur" <+> pretty x <+> ", maxOccur" <+> pretty y


data FunctionAttr x
    = FunctionAttr (SizeAttr x) PartialityAttr JectivityAttr
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
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
    = PartialityAttr_Partial
    | PartialityAttr_Total
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize PartialityAttr
instance Hashable  PartialityAttr
instance ToJSON    PartialityAttr where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  PartialityAttr where parseJSON = JSON.genericParseJSON jsonOptions
instance Default   PartialityAttr where def = PartialityAttr_Partial
instance Pretty    PartialityAttr where
    pretty PartialityAttr_Partial = prEmpty -- partial is the default
    pretty PartialityAttr_Total = "total"


data JectivityAttr
    = JectivityAttr_None
    | JectivityAttr_Injective
    | JectivityAttr_Surjective
    | JectivityAttr_Bijective
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize JectivityAttr
instance Hashable  JectivityAttr
instance ToJSON    JectivityAttr where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  JectivityAttr where parseJSON = JSON.genericParseJSON jsonOptions
instance Default   JectivityAttr where def = JectivityAttr_None
instance Pretty    JectivityAttr where
    pretty JectivityAttr_None = prEmpty
    pretty JectivityAttr_Injective = "injective"
    pretty JectivityAttr_Surjective = "surjective"
    pretty JectivityAttr_Bijective = "bijective"


data RelationAttr a = RelationAttr (SizeAttr a)
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize a => Serialize (RelationAttr a)
instance Hashable  a => Hashable  (RelationAttr a)
instance ToJSON    a => ToJSON    (RelationAttr a) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (RelationAttr a) where parseJSON = JSON.genericParseJSON jsonOptions
instance Default (RelationAttr a) where def = RelationAttr def
instance Pretty a => Pretty (RelationAttr a) where
    pretty (RelationAttr SizeAttr_None) = prEmpty
    pretty (RelationAttr a) = prParens (pretty a)


data PartitionAttr a = PartitionAttr
    { participantsSize  :: SizeAttr a
    , partsNum          :: SizeAttr a
    , partsSize         :: SizeAttr a
    , isComplete        :: Bool
    , isRegular         :: Bool
    }
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize a => Serialize (PartitionAttr a)
instance Hashable  a => Hashable  (PartitionAttr a)
instance ToJSON    a => ToJSON    (PartitionAttr a) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (PartitionAttr a) where parseJSON = JSON.genericParseJSON jsonOptions
instance Default (PartitionAttr a) where def = PartitionAttr def def def False False
instance Pretty a => Pretty (PartitionAttr a) where
    pretty (PartitionAttr a b c d e) =
        let inside = filter (/=prEmpty) [ prettyA a
                                        , prettyB b
                                        , prettyC c
                                        , prettyD d
                                        , prettyE e
                                        ]

            prettyA = pretty

            prettyB SizeAttr_None = prEmpty
            prettyB (SizeAttr_Size       x  ) = "numParts"    <+> pretty x
            prettyB (SizeAttr_MinSize    x  ) = "minNumParts" <+> pretty x
            prettyB (SizeAttr_MaxSize    x  ) = "maxNumParts" <+> pretty x
            prettyB (SizeAttr_MinMaxSize x y) = "minNumParts" <+> pretty x <+> ", maxNumParts" <+> pretty y

            prettyC SizeAttr_None = prEmpty
            prettyC (SizeAttr_Size       x  ) = "partSize"    <+> pretty x
            prettyC (SizeAttr_MinSize    x  ) = "minPartSize" <+> pretty x
            prettyC (SizeAttr_MaxSize    x  ) = "maxPartSize" <+> pretty x
            prettyC (SizeAttr_MinMaxSize x y) = "minPartSize" <+> pretty x <+> ", maxPartSize" <+> pretty y

            prettyD False = prEmpty
            prettyD True  = "complete"

            prettyE False = prEmpty
            prettyE True  = "regular"

        in  if null inside
                then prEmpty
                else prettyList prParens "," inside

data DomainAttributes a = DomainAttributes [DomainAttribute a]
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

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
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

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
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

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

    pretty (DomainEnum name (Just ranges) _) = pretty name <> prettyList prParens "," ranges
    pretty (DomainEnum name _             _) = pretty name

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

normaliseDomain :: Ord c => (c -> c) -> Domain r c -> Domain r c
normaliseDomain _norm DomainBool                  = DomainBool
normaliseDomain  norm (DomainInt rs             ) = DomainInt $ sort $ map (normaliseRange norm) rs
normaliseDomain _norm (DomainEnum n Nothing   mp) = DomainEnum n Nothing mp
normaliseDomain _norm (DomainEnum n (Just rs) mp) = DomainEnum n (Just $ sort rs) mp
normaliseDomain  norm (DomainUnnamed n x        ) = DomainUnnamed n (norm x)
normaliseDomain  norm (DomainTuple            doms     ) = DomainTuple $ map (normaliseDomain norm) doms
normaliseDomain  norm (DomainMatrix           dom1 dom2) = DomainMatrix      (normaliseDomain norm dom1)
                                                                             (normaliseDomain norm dom2)
normaliseDomain  norm (DomainSet       r attr dom      ) = DomainSet       r (fmap norm attr)
                                                                             (normaliseDomain norm dom)
normaliseDomain  norm (DomainMSet      r attr dom      ) = DomainMSet      r (fmap norm attr)
                                                                             (normaliseDomain norm dom)
normaliseDomain  norm (DomainFunction  r attr dom1 dom2) = DomainFunction  r (fmap norm attr)
                                                                             (normaliseDomain norm dom1)
                                                                             (normaliseDomain norm dom2)
normaliseDomain  norm (DomainRelation  r attr doms     ) = DomainRelation  r (fmap norm attr)
                                                                             (map (normaliseDomain norm) doms)
normaliseDomain  norm (DomainPartition r attr dom      ) = DomainPartition r (fmap norm attr)
                                                                             (normaliseDomain norm dom)
normaliseDomain _norm d = d

normaliseRange :: Ord c => (c -> c) -> Range c -> Range c
normaliseRange _norm RangeOpen             = RangeOpen
normaliseRange  norm (RangeSingle x)       = RangeBounded (norm x) (norm x)
normaliseRange  norm (RangeLowerBounded x) = RangeLowerBounded (norm x)
normaliseRange  norm (RangeUpperBounded x) = RangeUpperBounded (norm x)
normaliseRange  norm (RangeBounded x y)    = RangeBounded (norm x) (norm y)
