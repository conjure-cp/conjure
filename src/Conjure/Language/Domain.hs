{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Conjure.Language.Domain
    ( Domain(..)
    , HasRepresentation(..)
    , Range(..), rangesInts
    , SetAttr(..), SizeAttr(..)
    , MSetAttr(..), OccurAttr(..)
    , FunctionAttr(..), PartialityAttr(..), JectivityAttr(..)
    , RelationAttr(..), BinaryRelationAttrs(..), BinaryRelationAttr(..)
    , PartitionAttr(..)
    , AttrName(..)
    , DomainAttributes(..), DomainAttribute(..)         -- only for parsing
    , isPrimitiveDomain, domainCanIndexMatrix, getIndices
    , Tree(..), reprTree, reprAtTopLevel, applyReprTree
    , forgetRepr, changeRepr, defRepr
    , mkDomainBool, mkDomainInt, mkDomainIntB
    , typeOfDomain
    , readBinRel
    , allSupportedAttributes
    , addAttributeToDomain, addAttributesToDomain
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

-- QuickCheck
import Test.QuickCheck ( Arbitrary(..), choose, oneof, vectorOf, sized )

-- containers
import Data.Set as S ( Set, empty, toList, singleton, union )


data Domain r x
    = DomainBool
    | DomainInt [Range x]
    | DomainEnum
        Name
        (Maybe [Range Name])        -- subset of values for this domain
                                    -- Nothing *only* when GivenDomainDefnEnum and not LettingDomainDefnEnum
        (Maybe [(Name, Integer)])   -- the mapping to integers, if available
    | DomainUnnamed Name x
    | DomainTuple [Domain r x]
    | DomainRecord [(Name, Domain r x)]
    | DomainVariant [(Name, Domain r x)]
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

mkDomainBool :: Domain () x
mkDomainBool = DomainBool

mkDomainInt :: [Range x] -> Domain () x
mkDomainInt = DomainInt

mkDomainIntB :: x -> x -> Domain () x
mkDomainIntB l u = DomainInt [RangeBounded l u]

instance (Serialize r, Serialize x) => Serialize (Domain r x)
instance (Hashable  r, Hashable  x) => Hashable  (Domain r x)
instance (ToJSON    r, ToJSON    x) => ToJSON    (Domain r x) where toJSON = genericToJSON jsonOptions
instance (FromJSON  r, FromJSON  x) => FromJSON  (Domain r x) where parseJSON = genericParseJSON jsonOptions

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
typeOfDomain (DomainRecord        xs ) = TypeRecord     <$> sequence [ do t <- typeOf d ; return (n, t)
                                                                     | (n,d) <- xs ]
typeOfDomain (DomainVariant       xs ) = TypeVariant    <$> sequence [ do t <- typeOf d ; return (n, t)
                                                                     | (n,d) <- xs ]
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

forgetRepr :: (Pretty r, Pretty x) => Domain r x -> Domain () x
forgetRepr = defRepr

defRepr :: (Default r2, Pretty r, Pretty x) => Domain r x -> Domain r2 x
defRepr = changeRepr def

changeRepr :: (Pretty r, Pretty x) => r2 -> Domain r x -> Domain r2 x
changeRepr rep = go
    where
        go DomainBool = DomainBool
        go (DomainInt rs) = DomainInt rs
        go (DomainEnum defn rs mp) = DomainEnum defn rs mp
        go (DomainUnnamed defn s) = DomainUnnamed defn s
        go (DomainTuple ds) = DomainTuple (map go ds)
        go (DomainRecord xs) = DomainRecord (map (second go) xs)
        go (DomainVariant xs) = DomainVariant (map (second go) xs)
        go (DomainMatrix index inner) = DomainMatrix index (go inner)
        go (DomainSet _   attr d) =
            DomainSet rep attr (go d)
        go (DomainMSet _   attr d) =
            DomainMSet rep attr (go d)
        go (DomainFunction _   attr d1 d2) =
            DomainFunction rep attr (go d1) (go d2)
        go (DomainRelation _   attr ds) =
            DomainRelation rep attr (map go ds)
        go (DomainPartition _   attr d) =
            DomainPartition rep attr (go d)
        go (DomainOp op ds) = DomainOp op (map go ds)
        go (DomainReference x r) = DomainReference x (fmap go r)
        go (DomainMetaVar x) = DomainMetaVar x


data Tree a = Tree { rootLabel :: a, subForest :: [Tree a] }
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize a => Serialize (Tree a)
instance Hashable  a => Hashable  (Tree a)
instance ToJSON    a => ToJSON    (Tree a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (Tree a) where parseJSON = genericParseJSON jsonOptions

reprTree :: Domain r x -> Tree (Maybe r)
reprTree DomainBool{}    = Tree Nothing []
reprTree DomainInt{}     = Tree Nothing []
reprTree DomainEnum{}    = Tree Nothing []
reprTree DomainUnnamed{} = Tree Nothing []
reprTree (DomainTuple  as ) = Tree Nothing (map reprTree as)
reprTree (DomainRecord as ) = Tree Nothing (map (reprTree . snd) as)
reprTree (DomainVariant as) = Tree Nothing (map (reprTree . snd) as)
reprTree (DomainMatrix _ a) = Tree Nothing [reprTree a]
reprTree (DomainSet       r _ a  ) = Tree (Just r) [reprTree a]
reprTree (DomainMSet      r _ a  ) = Tree (Just r) [reprTree a]
reprTree (DomainFunction  r _ a b) = Tree (Just r) [reprTree a, reprTree b]
reprTree (DomainRelation  r _ as ) = Tree (Just r) (map reprTree as)
reprTree (DomainPartition r _ a  ) = Tree (Just r) [reprTree a]
reprTree DomainOp{}        = Tree Nothing []
reprTree DomainReference{} = Tree Nothing []
reprTree DomainMetaVar{}   = Tree Nothing []

reprAtTopLevel :: Domain r x -> Maybe r
reprAtTopLevel = rootLabel . reprTree

applyReprTree :: (MonadFail m, Pretty x, Pretty r2, Default r) => Domain r2 x -> Tree (Maybe r) -> m (Domain r x)
applyReprTree dom@DomainBool{}    (Tree Nothing []) = return (defRepr dom)
applyReprTree dom@DomainInt{}     (Tree Nothing []) = return (defRepr dom)
applyReprTree dom@DomainEnum{}    (Tree Nothing []) = return (defRepr dom)
applyReprTree dom@DomainUnnamed{} (Tree Nothing []) = return (defRepr dom)
applyReprTree (DomainTuple as  ) (Tree Nothing asRepr) =
    DomainTuple <$> zipWithM applyReprTree as asRepr
applyReprTree (DomainRecord as ) (Tree Nothing asRepr) =
    (DomainRecord  . zip (map fst as)) <$> zipWithM applyReprTree (map snd as) asRepr
applyReprTree (DomainVariant as) (Tree Nothing asRepr) =
    (DomainVariant . zip (map fst as)) <$> zipWithM applyReprTree (map snd as) asRepr
applyReprTree (DomainMatrix b a) (Tree Nothing [aRepr]) = DomainMatrix b <$> applyReprTree a aRepr
applyReprTree (DomainSet       _ attr a  ) (Tree (Just r) [aRepr]) = DomainSet r attr <$> applyReprTree a aRepr
applyReprTree (DomainMSet      _ attr a  ) (Tree (Just r) [aRepr]) = DomainMSet r attr <$> applyReprTree a aRepr
applyReprTree (DomainFunction  _ attr a b) (Tree (Just r) [aRepr, bRepr]) = DomainFunction r attr <$> applyReprTree a aRepr <*> applyReprTree b bRepr
applyReprTree (DomainRelation  _ attr as ) (Tree (Just r) asRepr) = DomainRelation r attr <$> zipWithM applyReprTree as asRepr
applyReprTree (DomainPartition _ attr a  ) (Tree (Just r) [aRepr]) = DomainPartition r attr <$> applyReprTree a aRepr
applyReprTree dom@DomainOp{}        (Tree Nothing []) = return (defRepr dom)
applyReprTree dom@DomainReference{} (Tree Nothing []) = return (defRepr dom)
applyReprTree dom@DomainMetaVar{}   (Tree Nothing []) = return (defRepr dom)
applyReprTree dom _ = fail $ "applyReprTree:" <++> pretty dom

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
-- attribute-as-constraint handling --------------------------------------------
--------------------------------------------------------------------------------

data AttrName
    = AttrName_size
    | AttrName_minSize
    | AttrName_maxSize
    | AttrName_minOccur
    | AttrName_maxOccur
    | AttrName_numParts
    | AttrName_minNumParts
    | AttrName_maxNumParts
    | AttrName_partSize
    | AttrName_minPartSize
    | AttrName_maxPartSize
    | AttrName_total
    | AttrName_injective
    | AttrName_surjective
    | AttrName_bijective
    | AttrName_complete
    | AttrName_regular
    -- bin rel ones
    | AttrName_reflexive
    | AttrName_irreflexive
    | AttrName_coreflexive
    | AttrName_symmetric
    | AttrName_antiSymmetric
    | AttrName_aSymmetric
    | AttrName_transitive
    | AttrName_Euclidean
    | AttrName_serial
    | AttrName_equivalence
    | AttrName_partialOrder
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize AttrName
instance Hashable  AttrName
instance ToJSON    AttrName where toJSON = genericToJSON jsonOptions
instance FromJSON  AttrName where parseJSON = genericParseJSON jsonOptions

instance Pretty AttrName where
    pretty AttrName_size = "size"
    pretty AttrName_minSize = "minSize"
    pretty AttrName_maxSize = "maxSize"
    pretty AttrName_minOccur = "minOccur"
    pretty AttrName_maxOccur = "maxOccur"
    pretty AttrName_numParts = "numParts"
    pretty AttrName_minNumParts = "minNumParts"
    pretty AttrName_maxNumParts = "maxNumParts"
    pretty AttrName_partSize = "partSize"
    pretty AttrName_minPartSize = "minPartSize"
    pretty AttrName_maxPartSize = "maxPartSize"
    pretty AttrName_total = "total"
    pretty AttrName_injective = "injective"
    pretty AttrName_surjective = "surjective"
    pretty AttrName_bijective = "bijective"
    pretty AttrName_complete = "complete"
    pretty AttrName_regular = "regular"
    pretty AttrName_reflexive = "reflexive"
    pretty AttrName_irreflexive = "irreflexive"
    pretty AttrName_coreflexive = "coreflexive"
    pretty AttrName_symmetric = "symmetric"
    pretty AttrName_antiSymmetric = "antiSymmetric"
    pretty AttrName_aSymmetric = "aSymmetric"
    pretty AttrName_transitive = "transitive"
    pretty AttrName_Euclidean = "Euclidean"
    pretty AttrName_serial = "serial"
    pretty AttrName_equivalence = "equivalence"
    pretty AttrName_partialOrder = "partialOrder"

instance IsString AttrName where
    fromString "size" = AttrName_size
    fromString "minSize" = AttrName_minSize
    fromString "maxSize" = AttrName_maxSize
    fromString "minOccur" = AttrName_minOccur
    fromString "maxOccur" = AttrName_maxOccur
    fromString "numParts" = AttrName_numParts
    fromString "minNumParts" = AttrName_minNumParts
    fromString "maxNumParts" = AttrName_maxNumParts
    fromString "partSize" = AttrName_partSize
    fromString "minPartSize" = AttrName_minPartSize
    fromString "maxPartSize" = AttrName_maxPartSize
    fromString "total" = AttrName_total
    fromString "injective" = AttrName_injective
    fromString "surjective" = AttrName_surjective
    fromString "bijective" = AttrName_bijective
    fromString "complete" = AttrName_complete
    fromString "regular" = AttrName_regular
    fromString "reflexive" = AttrName_reflexive
    fromString "irreflexive" = AttrName_irreflexive
    fromString "coreflexive" = AttrName_coreflexive
    fromString "symmetric" = AttrName_symmetric
    fromString "antiSymmetric" = AttrName_antiSymmetric
    fromString "aSymmetric" = AttrName_aSymmetric
    fromString "transitive" = AttrName_transitive
    fromString "Euclidean" = AttrName_Euclidean
    fromString "serial" = AttrName_serial
    fromString "equivalence" = AttrName_equivalence
    fromString "partialOrder" = AttrName_partialOrder
    fromString s = bug $ "fromString{AttrName}:" <+> pretty s


allSupportedAttributes :: [(Name, Int)]
allSupportedAttributes =
    map (,1) [ "size", "minSize", "maxSize"
             , "minOccur", "maxOccur"
             , "numParts", "minNumParts", "maxNumParts"
             , "partSize", "minPartSize", "maxPartSize"
             ] ++
    map (,0) [ "total"
             , "injective", "surjective", "bijective"
             , "complete", "regular"
             ] ++
    map (,0) [ "reflexive"
             , "irreflexive"
             , "coreflexive"
             , "symmetric"
             , "antiSymmetric"
             , "aSymmetric"
             , "transitive"
             , "total"
             , "Euclidean"
             , "serial"
             , "equivalence"
             , "partialOrder"
             ]


addAttributesToDomain
    :: (MonadFail m, Pretty r, Pretty x)
    => Domain r x
    -> [(AttrName, Maybe x)]
    -> m (Domain r x)
addAttributesToDomain domain [] = return domain
addAttributesToDomain domain ((attr, val) : rest) = do
    domain' <- addAttributeToDomain domain attr val
    addAttributesToDomain domain' rest


addAttributeToDomain
    :: (MonadFail m, Pretty r, Pretty x)
    => Domain r x                                   -- the input domain
    -> AttrName                                     -- the name of the attribute
    -> Maybe x                                      -- the value for the attribute
    -> m (Domain r x)                               -- the modified domain

addAttributeToDomain d@DomainBool{}      = const $ const $ return d
addAttributeToDomain d@DomainInt{}       = const $ const $ return d
addAttributeToDomain d@DomainEnum{}      = const $ const $ return d
addAttributeToDomain d@DomainUnnamed{}   = const $ const $ return d
addAttributeToDomain d@DomainTuple{}     = const $ const $ return d
addAttributeToDomain d@DomainRecord{}    = const $ const $ return d
addAttributeToDomain d@DomainVariant{}   = const $ const $ return d
addAttributeToDomain d@DomainMatrix{}    = const $ const $ return d
addAttributeToDomain d@DomainOp{}        = const $ const $ return d
addAttributeToDomain d@DomainReference{} = const $ const $ return d
addAttributeToDomain d@DomainMetaVar{}   = const $ const $ return d

addAttributeToDomain domain@(DomainSet r (SetAttr sizeAttr) inner) = updater where
    updater attr (Just val) = case attr of
        AttrName_size ->
            case sizeAttr of
                SizeAttr_Size{} -> fail $ "Cannot add a size attribute to this domain:" <++> pretty domain
                _               -> return $ DomainSet r (SetAttr (SizeAttr_Size val)) inner
        AttrName_minSize -> do
            let fails = fail $ "Cannot add a minSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MinSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainSet r (SetAttr (SizeAttr_MinSize val)) inner
                SizeAttr_MaxSize maxS -> return $ DomainSet r (SetAttr (SizeAttr_MinMaxSize val maxS)) inner
        AttrName_maxSize -> do
            let fails = fail $ "Cannot add a maxSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MaxSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainSet r (SetAttr (SizeAttr_MaxSize val)) inner
                SizeAttr_MinSize minS -> return $ DomainSet r (SetAttr (SizeAttr_MinMaxSize minS val)) inner
        _ ->
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
    updater attr _ =
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]

addAttributeToDomain domain@(DomainMSet r (MSetAttr sizeAttr occurAttr) inner) = updater where
    updater attr (Just val) = case attr of
        AttrName_size ->
            case sizeAttr of
                SizeAttr_Size{} -> fail $ "Cannot add a size attribute to this domain:" <++> pretty domain
                _               -> return $ DomainMSet r (MSetAttr (SizeAttr_Size val) occurAttr) inner
        AttrName_minSize -> do
            let fails = fail $ "Cannot add a minSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MinSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainMSet r
                                            (MSetAttr (SizeAttr_MinSize val)         occurAttr)
                                            inner
                SizeAttr_MaxSize maxS -> return $ DomainMSet r
                                            (MSetAttr (SizeAttr_MinMaxSize val maxS) occurAttr)
                                            inner
        AttrName_maxSize -> do
            let fails = fail $ "Cannot add a maxSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MaxSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainMSet r
                                            (MSetAttr (SizeAttr_MaxSize val)         occurAttr)
                                            inner
                SizeAttr_MinSize minS -> return $ DomainMSet r
                                            (MSetAttr (SizeAttr_MinMaxSize minS val) occurAttr)
                                            inner
        AttrName_minOccur -> do
            let fails = fail $ "Cannot add a minOccur attribute to this domain:" <++> pretty domain
            case occurAttr of
                OccurAttr_MinOccur{}    -> fails
                OccurAttr_MinMaxOccur{} -> fails
                OccurAttr_None          -> return $ DomainMSet r
                                            (MSetAttr sizeAttr (OccurAttr_MinOccur val))
                                            inner
                OccurAttr_MaxOccur maxO -> return $ DomainMSet r
                                            (MSetAttr sizeAttr (OccurAttr_MinMaxOccur val maxO))
                                            inner
        AttrName_maxOccur -> do
            let fails = fail $ "Cannot add a maxOccur attribute to this domain:" <++> pretty domain
            case occurAttr of
                OccurAttr_MaxOccur{}    -> fails
                OccurAttr_MinMaxOccur{} -> fails
                OccurAttr_None          -> return $ DomainMSet r
                                            (MSetAttr sizeAttr (OccurAttr_MaxOccur val))
                                            inner
                OccurAttr_MinOccur minO -> return $ DomainMSet r
                                            (MSetAttr sizeAttr (OccurAttr_MinMaxOccur minO val))
                                            inner
        _ ->
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
    updater attr _ =
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]

addAttributeToDomain domain@(DomainFunction r
                            (FunctionAttr sizeAttr partialityAttr jectivityAttr)
                            inF inT) = updater where
    updater attr (Just val) = case attr of
        AttrName_size ->
            case sizeAttr of
                SizeAttr_Size{} -> fail $ "Cannot add a size attribute to this domain:" <++> pretty domain
                _               -> return $ DomainFunction r
                                            (FunctionAttr (SizeAttr_Size val) partialityAttr jectivityAttr)
                                            inF inT
        AttrName_minSize -> do
            let fails = fail $ "Cannot add a minSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MinSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainFunction r
                                            (FunctionAttr (SizeAttr_MinSize val) partialityAttr jectivityAttr)
                                            inF inT
                SizeAttr_MaxSize maxS -> return $ DomainFunction r
                                            (FunctionAttr (SizeAttr_MinMaxSize val maxS) partialityAttr jectivityAttr)
                                            inF inT
        AttrName_maxSize -> do
            let fails = fail $ "Cannot add a maxSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MaxSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainFunction r
                                            (FunctionAttr (SizeAttr_MaxSize val) partialityAttr jectivityAttr)
                                            inF inT
                SizeAttr_MinSize minS -> return $ DomainFunction r
                                            (FunctionAttr (SizeAttr_MinMaxSize minS val) partialityAttr jectivityAttr)
                                            inF inT
        _ ->
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
    updater "total" Nothing = return $ DomainFunction r
                                            (FunctionAttr sizeAttr PartialityAttr_Total jectivityAttr)
                                            inF inT
    updater "injective" Nothing = return $
        case jectivityAttr of
            JectivityAttr_None       -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Injective )
                                            inF inT
            JectivityAttr_Injective  -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Injective )
                                            inF inT
            JectivityAttr_Surjective -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Bijective )
                                            inF inT
            JectivityAttr_Bijective  -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Bijective )
                                            inF inT
    updater "surjective" Nothing = return $
        case jectivityAttr of
            JectivityAttr_None          -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Surjective)
                                            inF inT
            JectivityAttr_Injective     -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Bijective )
                                            inF inT
            JectivityAttr_Surjective    -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Surjective)
                                            inF inT
            JectivityAttr_Bijective     -> DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Bijective )
                                            inF inT
    updater "bijective" Nothing = return $ DomainFunction r
                                            (FunctionAttr sizeAttr partialityAttr JectivityAttr_Bijective)
                                            inF inT
    updater attr _ =
        fail $ vcat [ "Unsupported attribute" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]

addAttributeToDomain domain@(DomainRelation r
                            (RelationAttr sizeAttr binRelAttr)
                            inners) = updater where
    supportedBinRel :: [AttrName]
    supportedBinRel =
        [ "reflexive", "irreflexive", "coreflexive"
        , "symmetric", "antiSymmetric", "aSymmetric"
        , "transitive", "total", "Euclidean"
        , "serial", "equivalence", "partialOrder"
        ]
    updater attr (Just val) = case attr of
        AttrName_size ->
            case sizeAttr of
                SizeAttr_Size{} -> fail $ "Cannot add a size attribute to this domain:" <++> pretty domain
                _               -> return $ DomainRelation r (RelationAttr (SizeAttr_Size val) binRelAttr) inners
        AttrName_minSize -> do
            let fails = fail $ "Cannot add a minSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MinSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainRelation r
                                            (RelationAttr (SizeAttr_MinSize val)         binRelAttr)
                                            inners
                SizeAttr_MaxSize maxS -> return $ DomainRelation r
                                            (RelationAttr (SizeAttr_MinMaxSize val maxS) binRelAttr)
                                            inners
        AttrName_maxSize -> do
            let fails = fail $ "Cannot add a maxSize attribute to this domain:" <++> pretty domain
            case sizeAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MaxSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainRelation r
                                            (RelationAttr (SizeAttr_MaxSize val)         binRelAttr)
                                            inners
                SizeAttr_MinSize minS -> return $ DomainRelation r
                                            (RelationAttr (SizeAttr_MinMaxSize minS val) binRelAttr)
                                            inners
        _ ->
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
    updater attr Nothing | attr `elem` supportedBinRel = case readBinRel attr of
        Nothing ->
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
        Just a  -> return $ DomainRelation r
                                (RelationAttr sizeAttr (binRelAttr `mappend` BinaryRelationAttrs (S.singleton a)))
                                inners
    updater attr _ =
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]

addAttributeToDomain domain@(DomainPartition r partitionAttr inner) = updater where
    updater attr (Just val) = case attr of

        AttrName_size ->
            case participantsSize partitionAttr of
                SizeAttr_Size{} -> fail $ "Cannot add a size attribute to this domain:" <++> pretty domain
                _               -> return $ DomainPartition r
                                            (partitionAttr { participantsSize = SizeAttr_Size val })
                                            inner
        AttrName_minSize -> do
            let fails = fail $ "Cannot add a minSize attribute to this domain:" <++> pretty domain
            case participantsSize partitionAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MinSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainPartition r
                                            (partitionAttr { participantsSize = SizeAttr_MinSize val })
                                            inner
                SizeAttr_MaxSize maxS -> return $ DomainPartition r
                                            (partitionAttr { participantsSize = SizeAttr_MinMaxSize val maxS })
                                            inner
        AttrName_maxSize -> do
            let fails = fail $ "Cannot add a maxSize attribute to this domain:" <++> pretty domain
            case participantsSize partitionAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MaxSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainPartition r
                                            (partitionAttr { participantsSize = SizeAttr_MaxSize val })
                                            inner
                SizeAttr_MinSize minS -> return $ DomainPartition r
                                            (partitionAttr { participantsSize = SizeAttr_MinMaxSize minS val })
                                            inner

        AttrName_numParts ->
            case partsNum partitionAttr of
                SizeAttr_Size{} -> fail $ "Cannot add a numParts attribute to this domain:" <++> pretty domain
                _               -> return $ DomainPartition r (partitionAttr { partsNum = SizeAttr_Size val }) inner
        AttrName_minNumParts -> do
            let fails = fail $ "Cannot add a minNumParts attribute to this domain:" <++> pretty domain
            case partsNum partitionAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MinSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainPartition r
                                            (partitionAttr { partsNum = SizeAttr_MinSize val })
                                            inner
                SizeAttr_MaxSize maxS -> return $ DomainPartition r
                                            (partitionAttr { partsNum = SizeAttr_MinMaxSize val maxS })
                                            inner
        AttrName_maxNumParts -> do
            let fails = fail $ "Cannot add a maxNumParts attribute to this domain:" <++> pretty domain
            case partsNum partitionAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MaxSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainPartition r
                                            (partitionAttr { partsNum = SizeAttr_MaxSize val })
                                            inner
                SizeAttr_MinSize minS -> return $ DomainPartition r
                                            (partitionAttr { partsNum = SizeAttr_MinMaxSize minS val })
                                            inner

        AttrName_partSize ->
            case partsSize partitionAttr of
                SizeAttr_Size{} -> fail $ "Cannot add a partSize attribute to this domain:" <++> pretty domain
                _               -> return $ DomainPartition r (partitionAttr { partsSize = SizeAttr_Size val }) inner
        AttrName_minPartSize -> do
            let fails = fail $ "Cannot add a minPartSize attribute to this domain:" <++> pretty domain
            case partsSize partitionAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MinSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainPartition r
                                            (partitionAttr { partsSize = SizeAttr_MinSize val })
                                            inner
                SizeAttr_MaxSize maxS -> return $ DomainPartition r
                                            (partitionAttr { partsSize = SizeAttr_MinMaxSize val maxS })
                                            inner
        AttrName_maxPartSize -> do
            let fails = fail $ "Cannot add a maxPartSize attribute to this domain:" <++> pretty domain
            case partsSize partitionAttr of
                SizeAttr_Size{}       -> fails
                SizeAttr_MaxSize{}    -> fails
                SizeAttr_MinMaxSize{} -> fails
                SizeAttr_None{}       -> return $ DomainPartition r
                                            (partitionAttr { partsSize = SizeAttr_MaxSize val })
                                            inner
                SizeAttr_MinSize minS -> return $ DomainPartition r
                                            (partitionAttr { partsSize = SizeAttr_MinMaxSize minS val })
                                            inner

        _ ->
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]
    updater AttrName_complete Nothing =
            return $ DomainPartition r (partitionAttr { isComplete = True }) inner
    updater AttrName_regular Nothing =
            return $ DomainPartition r (partitionAttr { isRegular  = True }) inner
    updater attr _ =
            fail $ vcat [ "Unsupported attribute" <+> pretty attr
                        , "For the domain:" <+> pretty domain
                        ]


--------------------------------------------------------------------------------
-- attribute definitions -------------------------------------------------------
--------------------------------------------------------------------------------

data SetAttr a = SetAttr (SizeAttr a)
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize a => Serialize (SetAttr a)
instance Hashable  a => Hashable  (SetAttr a)
instance ToJSON    a => ToJSON    (SetAttr a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (SetAttr a) where parseJSON = genericParseJSON jsonOptions
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
instance ToJSON    a => ToJSON    (SizeAttr a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (SizeAttr a) where parseJSON = genericParseJSON jsonOptions
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
instance ToJSON    a => ToJSON    (MSetAttr a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (MSetAttr a) where parseJSON = genericParseJSON jsonOptions
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
instance ToJSON    a => ToJSON    (OccurAttr a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (OccurAttr a) where parseJSON = genericParseJSON jsonOptions
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
instance ToJSON    a => ToJSON    (FunctionAttr a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (FunctionAttr a) where parseJSON = genericParseJSON jsonOptions
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
instance ToJSON    PartialityAttr where toJSON = genericToJSON jsonOptions
instance FromJSON  PartialityAttr where parseJSON = genericParseJSON jsonOptions
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
instance ToJSON    JectivityAttr where toJSON = genericToJSON jsonOptions
instance FromJSON  JectivityAttr where parseJSON = genericParseJSON jsonOptions
instance Default   JectivityAttr where def = JectivityAttr_None
instance Pretty    JectivityAttr where
    pretty JectivityAttr_None = prEmpty
    pretty JectivityAttr_Injective = "injective"
    pretty JectivityAttr_Surjective = "surjective"
    pretty JectivityAttr_Bijective = "bijective"


data RelationAttr a = RelationAttr (SizeAttr a) BinaryRelationAttrs
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize a => Serialize (RelationAttr a)
instance Hashable  a => Hashable  (RelationAttr a)
instance ToJSON    a => ToJSON    (RelationAttr a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (RelationAttr a) where parseJSON = genericParseJSON jsonOptions
instance Default (RelationAttr a) where def = RelationAttr def def
instance Pretty a => Pretty (RelationAttr a) where
    pretty (RelationAttr a b) =
        let inside = filter (/=prEmpty) [pretty a, pretty b]
        in  if null inside
                then prEmpty
                else prettyList prParens "," inside


newtype BinaryRelationAttrs = BinaryRelationAttrs (S.Set BinaryRelationAttr)
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize BinaryRelationAttrs
instance Hashable  BinaryRelationAttrs where hashWithSalt salt (BinaryRelationAttrs a) = hashWithSalt salt (S.toList a)
instance ToJSON    BinaryRelationAttrs where toJSON = genericToJSON jsonOptions
instance FromJSON  BinaryRelationAttrs where parseJSON = genericParseJSON jsonOptions
instance Default   BinaryRelationAttrs where def = BinaryRelationAttrs S.empty
instance Pretty BinaryRelationAttrs where
    pretty (BinaryRelationAttrs attrs) = prettyList id "," (S.toList attrs)
instance Monoid BinaryRelationAttrs where
    mempty = BinaryRelationAttrs S.empty
    mappend (BinaryRelationAttrs a) (BinaryRelationAttrs b) = BinaryRelationAttrs (S.union a b)


data BinaryRelationAttr
    = BinRelAttr_Reflexive
    | BinRelAttr_Irreflexive
    | BinRelAttr_Coreflexive
    | BinRelAttr_Symmetric
    | BinRelAttr_AntiSymmetric
    | BinRelAttr_ASymmetric
    | BinRelAttr_Transitive
    | BinRelAttr_Total
    | BinRelAttr_Euclidean
    | BinRelAttr_Serial
    | BinRelAttr_Equivalence
    | BinRelAttr_PartialOrder
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize BinaryRelationAttr
instance Hashable  BinaryRelationAttr
instance ToJSON    BinaryRelationAttr where toJSON = genericToJSON jsonOptions
instance FromJSON  BinaryRelationAttr where parseJSON = genericParseJSON jsonOptions
instance Pretty BinaryRelationAttr where
    pretty BinRelAttr_Reflexive     = "reflexive"
    pretty BinRelAttr_Irreflexive   = "irreflexive"
    pretty BinRelAttr_Coreflexive   = "coreflexive"
    pretty BinRelAttr_Symmetric     = "symmetric"
    pretty BinRelAttr_AntiSymmetric = "antiSymmetric"
    pretty BinRelAttr_ASymmetric    = "aSymmetric"
    pretty BinRelAttr_Transitive    = "transitive"
    pretty BinRelAttr_Total         = "total"
    pretty BinRelAttr_Euclidean     = "Euclidean"
    pretty BinRelAttr_Serial        = "serial"
    pretty BinRelAttr_Equivalence   = "equivalence"
    pretty BinRelAttr_PartialOrder  = "partialOrder"

readBinRel :: MonadFail m => AttrName -> m BinaryRelationAttr
readBinRel AttrName_reflexive     = return BinRelAttr_Reflexive
readBinRel AttrName_irreflexive   = return BinRelAttr_Irreflexive
readBinRel AttrName_coreflexive   = return BinRelAttr_Coreflexive
readBinRel AttrName_symmetric     = return BinRelAttr_Symmetric
readBinRel AttrName_antiSymmetric = return BinRelAttr_AntiSymmetric
readBinRel AttrName_aSymmetric    = return BinRelAttr_ASymmetric
readBinRel AttrName_transitive    = return BinRelAttr_Transitive
readBinRel AttrName_total         = return BinRelAttr_Total
readBinRel AttrName_Euclidean     = return BinRelAttr_Euclidean
readBinRel AttrName_serial        = return BinRelAttr_Serial
readBinRel AttrName_equivalence   = return BinRelAttr_Equivalence
readBinRel AttrName_partialOrder  = return BinRelAttr_PartialOrder
readBinRel a = fail $ "Not a binary relation attribute:" <+> pretty a

-- reflexive        forAll x : T . rel(x,x)
-- irreflexive      forAll x : T . !rel(x,x)
-- coreflexive      forAll x,y : T . rel(x,y) -> x = y
--
-- symmetric        forAll x,y : T . rel(x,y) -> rel(y,x)
-- antisymmetric    forAll x,y : T . rel(x,y) /\ rel(y,x) -> x = y
-- asymmetric       forAll x,y : T . rel(x,y) -> !rel(y,x)
--
-- transitive       forAll x,y,z : T . rel(x,y) /\ rel(y,z) -> rel(x,z)
--
-- total            forAll x,y : T . rel(x,y) \/ rel(y,x)
-- Euclidean        forAll x,y,z : T . rel(x,y) /\ rel(x,z) -> rel(y,z)
-- serial           forAll x : T . exists y : T . rel(x,y)
-- equivalence      reflexive + symmetric + transitive
-- partialOrder     reflexive + antisymmetric + transitive


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
instance ToJSON    a => ToJSON    (PartitionAttr a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (PartitionAttr a) where parseJSON = genericParseJSON jsonOptions
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
instance ToJSON    a => ToJSON    (DomainAttributes a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (DomainAttributes a) where parseJSON = genericParseJSON jsonOptions

instance Default (DomainAttributes a) where
    def = DomainAttributes []


data DomainAttribute a
    = DAName Name
    | DANameValue Name a
    | DADotDot
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize a => Serialize (DomainAttribute a)
instance Hashable  a => Hashable  (DomainAttribute a)
instance ToJSON    a => ToJSON    (DomainAttribute a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON  (DomainAttribute a) where parseJSON = genericParseJSON jsonOptions


data Range a
    = RangeOpen
    | RangeSingle a
    | RangeLowerBounded a
    | RangeUpperBounded a
    | RangeBounded a a
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize a => Serialize (Range a)
instance Hashable  a => Hashable (Range a)
instance ToJSON    a => ToJSON (Range a) where toJSON = genericToJSON jsonOptions
instance FromJSON  a => FromJSON (Range a) where parseJSON = genericParseJSON jsonOptions

instance Arbitrary a => Arbitrary (Range a) where
    arbitrary = oneof
        [ return RangeOpen
        , RangeSingle <$> arbitrary
        , RangeLowerBounded <$> arbitrary
        , RangeUpperBounded <$> arbitrary
        , RangeBounded <$> arbitrary <*> arbitrary
        ]

rangesInts :: (MonadFail m, ExpressionLike c) => [Range c] -> m [Integer]
rangesInts = liftM (sortNub . concat) . mapM rangeInts
    where
        rangeInts (RangeSingle x) = return <$> intOut x
        rangeInts (RangeBounded x y) = do x' <- intOut x
                                          y' <- intOut y
                                          return [x' .. y']
        rangeInts _ = fail "Infinite range (or not an integer range)"

expandRanges :: ExpressionLike c => [Range c] -> [Range c]
expandRanges r =
    case rangesInts r of
        Nothing -> r
        Just [] -> []
        Just is ->
            if [ minimum is .. maximum is ] == is
                then [RangeBounded (fromInt (minimum is)) (fromInt (maximum is))]
                else map (RangeSingle . fromInt) is


data HasRepresentation = NoRepresentation | HasRepresentation Name
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize HasRepresentation
instance Hashable  HasRepresentation
instance ToJSON    HasRepresentation where toJSON = genericToJSON jsonOptions
instance FromJSON  HasRepresentation where parseJSON = genericParseJSON jsonOptions

instance IsString  HasRepresentation where
    fromString = HasRepresentation . Name . T.pack

instance Default HasRepresentation where
    def = NoRepresentation

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

    pretty (DomainRecord xs) = "record" <+> prettyList prBraces ","
        [ pretty nm <+> ":" <+> pretty d | (nm, d) <- xs ]

    pretty (DomainVariant xs) = "variant" <+> prettyList prBraces ","
        [ pretty nm <+> ":" <+> pretty d | (nm, d) <- xs ]

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

normaliseDomain :: (Ord c, ExpressionLike c) => (c -> c) -> Domain r c -> Domain r c
normaliseDomain _norm DomainBool                  = DomainBool
normaliseDomain  norm (DomainInt rs             ) = DomainInt $ sort $ map (normaliseRange norm) (expandRanges rs)
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
