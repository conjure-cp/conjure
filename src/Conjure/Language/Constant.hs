{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Conjure.Language.Constant
    ( Constant(..)
    , valuesInIntDomain
    , normaliseConstant
    , mkUndef, isUndef
    , emptyCollection
    , viewConstantBool
    , viewConstantInt
    , viewConstantIntWithTag
    , viewConstantTuple
    , viewConstantRecord
    , viewConstantVariant
    , viewConstantMatrix
    , viewConstantSet
    , viewConstantMSet
    , viewConstantFunction
    , viewConstantSequence
    , viewConstantRelation
    , viewConstantPartition
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.UserError ( userErr1 )
import Conjure.Language.Name
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.AbstractLiteral

import Conjure.Language.DomainSizeOf
import Conjure.Language.TypeOf
import Conjure.Language.AdHoc
import Conjure.Language.Pretty

-- base
import Data.Data ( toConstr, constrIndex )

-- QuickCheck
import Test.QuickCheck ( Arbitrary(..), oneof )

-- aeson
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data Constant
    = ConstantBool Bool
    | ConstantInt IntTag Integer
    | ConstantEnum Name   {- name for the enum domain -}
                   [Name] {- values in the enum domain -}
                   Name   {- the literal -}
    | ConstantField Name Type                               -- the name of a field of Record or Variant and its type
    | ConstantFromJSON [Constant]                           -- for holding list-like stuff read from json input. they can act as values for many types, depending on context.
    | ConstantAbstract (AbstractLiteral Constant)
    | DomainInConstant (Domain () Constant)
    | TypedConstant Constant Type
    | ConstantUndefined Text Type                           -- never use this for a bool
                                                            -- use false instead for them
    deriving (Show, Data, Typeable, Generic)

instance Eq Constant where
    a == b = compare a b == EQ

-- implementing the Eq&Ord instances by hand, because we want to special case the TypedConstant constructor
instance Ord Constant where

    -- do not use type info when comparing
    compare (TypedConstant a _) (TypedConstant b _) = compare a b
    compare (TypedConstant a _) b = compare a b
    compare a (TypedConstant b _) = compare a b

    -- the "usual" comparisons
    compare (ConstantBool a) (ConstantBool b) = compare a b
    compare (ConstantInt _ a) (ConstantInt _ b) = compare a b
    compare (ConstantEnum _ aVals aVal) (ConstantEnum _ bVals bVal) =
        compare (elemIndex aVal aVals, aVal) (elemIndex bVal bVals, bVal)
    compare (ConstantField a1 a2) (ConstantField b1 b2) = compare (a1,a2) (b1,b2)
    compare (ConstantAbstract a) (ConstantAbstract b) = compare a b
    compare (DomainInConstant a) (DomainInConstant b) = compare a b
    compare (ConstantUndefined a1 a2) (ConstantUndefined b1 b2) = compare (a1,a2) (b1,b2)

    -- if the constructors do not match
    compare a b = compare (constrIndex (toConstr a)) (constrIndex (toConstr b))

instance Serialize Constant
instance Hashable  Constant
instance ToJSON    Constant where toJSON = genericToJSON jsonOptions
instance FromJSON  Constant where parseJSON = genericParseJSON jsonOptions

instance SimpleJSON Constant where
    toSimpleJSON c =
        case c of
            ConstantBool b -> return (toJSON b)
            ConstantInt _ i -> return (toJSON i)
            ConstantEnum _ _ nm -> return (toJSON (renderNormal nm))
            ConstantAbstract lit -> toSimpleJSON lit
            TypedConstant c' _ -> toSimpleJSON c'
            _ -> noToSimpleJSON c
    fromSimpleJSON x@JSON.Number{} = ConstantInt TagInt <$> fromSimpleJSON x
    fromSimpleJSON (JSON.Array xs) = do
        ys <- mapM fromSimpleJSON (V.toList xs)
        return $ ConstantFromJSON ys
    fromSimpleJSON (JSON.Object m) = do
        ys <- forM (M.toList m) $ \ (name, value) ->
            -- the name must be an integer
            -- and this is a function from ints we are reading here
            case readMay (textToString name) of
                Nothing -> userErr1 "This is not an int. Boo."
                Just a -> do
                    b <- fromSimpleJSON value
                    return (ConstantInt TagInt a, b)
        return $ ConstantAbstract $ AbsLitFunction ys
    fromSimpleJSON x = noFromSimpleJSON "Constant" x

instance ToFromMiniZinc Constant where
    toMiniZinc c =
        case c of
            ConstantBool b -> return (MZNBool b)
            ConstantInt _ i -> return (MZNInt i)
            ConstantAbstract lit -> toMiniZinc lit
            TypedConstant c' _ -> toMiniZinc c'
            _ -> noToMiniZinc c

instance Arbitrary Constant where
    arbitrary = oneof
        [ ConstantBool <$> arbitrary
        , ConstantInt TagInt <$> arbitrary
        ]

instance TypeOf Constant where
    typeOf ConstantBool{}             = return TypeBool
    typeOf (ConstantInt t _)          = return (TypeInt t)
    typeOf (ConstantEnum defn _ _ )   = return (TypeEnum defn)
    typeOf (ConstantField _ ty)       = return ty
    typeOf (ConstantAbstract x    )   = typeOf x
    typeOf (DomainInConstant dom)     = typeOfDomain dom
    typeOf (TypedConstant _ ty)       = return ty
    typeOf (ConstantUndefined _ ty)   = return ty
    typeOf ConstantFromJSON{}         = return TypeAny

instance DomainSizeOf Constant Integer where
    domainSizeOf DomainBool{} = return 2
    domainSizeOf (DomainIntE x) = bug ("not implemented, domainSizeOf DomainIntE" <+> pretty (show x))
    domainSizeOf (DomainInt _ rs) = domainSizeOfRanges rs
    domainSizeOf DomainEnum{} = fail "domainSizeOf: Unknown for given enum."
    domainSizeOf (DomainTuple ds) = product <$> mapM domainSizeOf ds
    domainSizeOf (DomainMatrix index inner) = intPow <$> domainSizeOf inner <*> domainSizeOf index
    domainSizeOf d@(DomainSet _ (SetAttr attrs) inner) =
        case attrs of
            SizeAttr_None -> do
                innerSize <- domainSizeOf inner
                return (2 `intPow` innerSize)
            SizeAttr_Size (ConstantInt _ size) -> do
                innerSize <- domainSizeOf inner
                return (nchoosek (product . enumFromTo 1) innerSize size)
            SizeAttr_MinSize{} -> do
                -- TODO: we can do better here
                innerSize <- domainSizeOf inner
                return (2 `intPow` innerSize)
            SizeAttr_MaxSize (ConstantInt _ maxSize) -> do
                innerSize <- domainSizeOf inner
                return $ sum [ nchoosek (product . enumFromTo 1) innerSize k | k <- [0 .. maxSize] ]
            SizeAttr_MinMaxSize (ConstantInt _ minSize) (ConstantInt _ maxSize) -> do
                innerSize <- domainSizeOf inner
                return $ sum [ nchoosek (product . enumFromTo 1) innerSize k | k <- [minSize .. maxSize] ]
            _ -> fail ("domainSizeOf{Constant}" <+> pretty d)
    domainSizeOf DomainMSet      {} = bug "not implemented: domainSizeOf DomainMSet"
    domainSizeOf DomainFunction  {} = bug "not implemented: domainSizeOf DomainFunction"
    domainSizeOf DomainRelation  {} = bug "not implemented: domainSizeOf DomainRelation"
    domainSizeOf DomainPartition {} = bug "not implemented: domainSizeOf DomainPartition"
    domainSizeOf _                  = bug "not implemented: domainSizeOf"

emptyCollection :: Constant -> Bool
emptyCollection ConstantBool{} = False
emptyCollection ConstantInt{} = False
emptyCollection ConstantEnum{} = False
emptyCollection ConstantField{} = False
emptyCollection (ConstantAbstract x) = emptyCollectionAbsLit x
emptyCollection DomainInConstant{} = False
emptyCollection (TypedConstant x _) = emptyCollection x
emptyCollection ConstantUndefined{} = False
emptyCollection ConstantFromJSON{} = False

intPow :: Integer -> Integer -> Integer
intPow = (^)

domainSizeOfRanges :: MonadFail m => [Range Constant] -> m Integer
domainSizeOfRanges = fmap genericLength . valuesInIntDomain

instance DomainSizeOf Constant Constant where
    domainSizeOf = fmap (ConstantInt TagInt) . domainSizeOf

instance Pretty Constant where

    pretty (TypedConstant (ConstantAbstract (AbsLitMatrix _ [])) ty) | TypeAny `elem` universe ty = "[]"

    -- hack, oh sweet hack!
    -- print a domain instead of a type when printing an empty matrix literal.
    -- this means we print "int()" instead of "int" inside the index of a matrix type
    -- SR expects it this way...
    pretty (TypedConstant (ConstantAbstract (AbsLitMatrix _ [])) ty) =
        let
            pretty' (TypeMatrix index innerNested)
                = "matrix indexed by" <+> prettyList prBrackets "," (map pretty' indices)
                                      <+> "of" <+> pretty inner
                where
                    (indices,inner) = first (index:) $ collect innerNested
                    collect (TypeMatrix i j) = first (i:) $ collect j
                    collect x = ([],x)
            pretty' TypeInt{} = "int()"
            pretty' t = pretty t
        in
            prParens $ "[] : `" <> pretty' ty <> "`"

    pretty (ConstantBool False)          = "false"
    pretty (ConstantBool True )          = "true"
    pretty (ConstantInt _ x   )          = pretty x
    pretty (ConstantEnum _ _ x)          = pretty x
    pretty (ConstantField n _)           = pretty n
    pretty (ConstantAbstract x)          = pretty x
    pretty (DomainInConstant d)          = "`" <> pretty d <> "`"
    pretty (TypedConstant x ty)          = prParens $ pretty x <+> ":" <+> "`" <> pretty ty <> "`"
    pretty (ConstantUndefined reason ty) = "undefined" <> prParens (pretty reason <+> ":" <+> "`" <> pretty ty <> "`")
    pretty (ConstantFromJSON xs) = "ConstantFromJSON[" <+> prettyList id "," xs <+> "]"

instance ExpressionLike Constant where
    fromInt = ConstantInt TagInt
    fromIntWithTag i t = ConstantInt t i
    intOut _ (ConstantInt _ x) = return x
    intOut doc c = fail $ vcat [ "Expecting an integer, but found:" <+> pretty c
                               , "Called from:" <+> doc
                               ]

    fromBool = ConstantBool
    boolOut (ConstantBool x) = return x
    boolOut ConstantUndefined{} = return False
    boolOut c = fail ("Expecting a boolean, but found:" <+> pretty c)

    fromList xs = ConstantAbstract $ AbsLitMatrix (mkDomainIntB 1 (fromInt $ genericLength xs)) xs
    listOut (ConstantAbstract (AbsLitMatrix _ xs)) = return xs
    listOut c = fail ("Expecting a matrix literal, but found:" <+> pretty c)

instance ReferenceContainer Constant where
    fromName name = bug ("ReferenceContainer{Constant} fromName --" <+> pretty name)
    nameOut (ConstantField nm _) = return nm
    nameOut p = bug ("ReferenceContainer{Constant} nameOut --" <+> pretty p)

instance DomainContainer Constant (Domain ()) where
    fromDomain = DomainInConstant
    domainOut (DomainInConstant dom) = return dom
    domainOut _ = fail "domainOut{Constant}"

mkUndef :: Type -> Doc -> Constant
mkUndef TypeBool _ = ConstantBool False
mkUndef ty reason = ConstantUndefined (stringToText $ show reason) ty

isUndef :: Constant -> Bool
isUndef ConstantUndefined{} = True
isUndef _ = False

normaliseConstant :: Constant -> Constant
normaliseConstant x@ConstantBool{} = x
normaliseConstant x@ConstantInt{}  = x
normaliseConstant x@ConstantEnum{} = x
normaliseConstant x@ConstantField{} = x
normaliseConstant (ConstantAbstract x) = ConstantAbstract (normaliseAbsLit normaliseConstant x)
normaliseConstant (DomainInConstant d) = DomainInConstant (normaliseDomain normaliseConstant d)
normaliseConstant (TypedConstant c ty) = TypedConstant (normaliseConstant c) ty
normaliseConstant x@ConstantUndefined{} = x
normaliseConstant (ConstantFromJSON xs) = ConstantFromJSON (map normaliseConstant xs)

instance Num Constant where
    ConstantInt _ x + ConstantInt _ y = ConstantInt TagInt (x+y)
    x + y = bug $ vcat [ "Num Constant (+)", "x:" <+> pretty x, "y:" <+> pretty y ]
    ConstantInt _ x - ConstantInt _ y = ConstantInt TagInt (x-y)
    x - y = bug $ vcat [ "Num Constant (-)", "x:" <+> pretty x, "y:" <+> pretty y ]
    ConstantInt _ x * ConstantInt _ y = ConstantInt TagInt (x*y)
    x * y = bug $ vcat [ "Num Constant (*)", "x:" <+> pretty x, "y:" <+> pretty y ]
    abs (ConstantInt t x) = ConstantInt t (abs x)
    abs x = bug $ vcat [ "Num Constant abs", "x:" <+> pretty x ]
    signum (ConstantInt t x) = ConstantInt t (signum x)
    signum x = bug $ vcat [ "Num Constant signum", "x:" <+> pretty x ]
    fromInteger = ConstantInt TagInt . fromInteger


valuesInIntDomain :: MonadFail m => [Range Constant] -> m [Integer]
valuesInIntDomain ranges =
    if isFinite
        then return allValues
        else fail $ "Expected finite integer ranges, but got:" <++> prettyList id "," ranges

    where

        allRanges :: [Maybe [Integer]]
        allRanges =
            [ vals
            | r <- ranges
            , let vals = case r of
                    RangeSingle (ConstantInt _ x) -> return [x]
                    RangeBounded (ConstantInt _ l) (ConstantInt _ u) -> return [l..u]
                    _ -> Nothing
            ]

        isFinite :: Bool
        isFinite = Nothing `notElem` allRanges

        allValues :: [Integer]
        allValues = sortNub $ concat $ catMaybes allRanges


viewConstantBool :: MonadFail m => Constant -> m Bool
viewConstantBool (ConstantBool i) = return i
viewConstantBool (ConstantInt _ 0) = return False
viewConstantBool (ConstantInt _ 1) = return True
viewConstantBool constant = fail ("Expecting a boolean, but got:" <++> pretty constant)

viewConstantInt :: MonadFail m => Constant -> m Integer
viewConstantInt (ConstantInt _ i) = return i
viewConstantInt constant = fail ("Expecting an integer, but got:" <++> pretty constant)

viewConstantIntWithTag :: MonadFail m => Constant -> m (IntTag, Integer)
viewConstantIntWithTag (ConstantInt t i) = return (t, i)
viewConstantIntWithTag constant = fail ("Expecting an integer, but got:" <++> pretty constant)

viewConstantTuple :: MonadFail m => Constant -> m [Constant]
viewConstantTuple (ConstantAbstract (AbsLitTuple xs)) = return xs
viewConstantTuple (TypedConstant c _) = viewConstantTuple c
viewConstantTuple (ConstantFromJSON xs) = return xs
viewConstantTuple constant = fail ("Expecting a tuple, but got:" <++> pretty constant)

viewConstantRecord :: MonadFail m => Constant -> m [(Name, Constant)]
viewConstantRecord (ConstantAbstract (AbsLitRecord xs)) = return (sortOn fst xs)
viewConstantRecord (TypedConstant c _) = viewConstantRecord c
viewConstantRecord constant = fail ("Expecting a record, but got:" <++> pretty constant)

viewConstantVariant :: MonadFail m => Constant -> m (Maybe [(Name, Domain () Constant)], Name, Constant)
viewConstantVariant (ConstantAbstract (AbsLitVariant lu nm x)) = return (lu, nm, x)
viewConstantVariant (TypedConstant c _) = viewConstantVariant c
viewConstantVariant constant = fail ("Expecting a variant, but got:" <++> pretty constant)

viewConstantMatrix :: MonadFail m => Constant -> m (Domain () Constant, [Constant])
viewConstantMatrix (ConstantAbstract (AbsLitMatrix ind xs)) = return (expandDomainReference ind, xs)
viewConstantMatrix (TypedConstant c _) = viewConstantMatrix c
viewConstantMatrix constant = fail ("Expecting a matrix, but got:" <++> pretty constant)

viewConstantSet :: MonadFail m => Constant -> m [Constant]
viewConstantSet (ConstantAbstract (AbsLitSet xs)) = return xs
viewConstantSet (TypedConstant c _) = viewConstantSet c
viewConstantSet (ConstantFromJSON xs) = return xs
viewConstantSet constant = fail ("Expecting a set, but got:" <++> pretty constant)

viewConstantMSet :: MonadFail m => Constant -> m [Constant]
viewConstantMSet (ConstantAbstract (AbsLitMSet xs)) = return xs
viewConstantMSet (TypedConstant c _) = viewConstantMSet c
viewConstantMSet (ConstantFromJSON xs) = return xs
viewConstantMSet constant = fail ("Expecting an mset, but got:" <++> pretty constant)

viewConstantFunction :: MonadFail m => Constant -> m [(Constant, Constant)]
viewConstantFunction (ConstantAbstract (AbsLitFunction xs)) = return xs
viewConstantFunction (TypedConstant c _) = viewConstantFunction c
viewConstantFunction constant = do
    let
        suggestion = case constant of
            ConstantAbstract (AbsLitMatrix (expandDomainReference -> DomainInt _ rs) vals) -> do
                froms <- valuesInIntDomain rs
                return $ Just $ pretty $ AbsLitFunction (zip (map (ConstantInt TagInt) froms) vals)
            _ -> return Nothing
    suggestion >>= \case
        Nothing  -> fail ("Expecting a function, but got:" <++> pretty constant)
        Just sug -> fail (vcat [ "Expecting a function, but got:" <++> pretty constant
                               , "Maybe you meant:" <++> sug
                               ])

viewConstantSequence :: MonadFail m => Constant -> m [Constant]
viewConstantSequence (ConstantAbstract (AbsLitSequence xs)) = return xs
viewConstantSequence (TypedConstant c _) = viewConstantSequence c
viewConstantSequence (ConstantFromJSON xs) = return xs
viewConstantSequence constant = fail ("Expecting a sequence, but got:" <++> pretty constant)

viewConstantRelation :: MonadFail m => Constant -> m [[Constant]]
viewConstantRelation (ConstantAbstract (AbsLitRelation xs)) = return xs
viewConstantRelation (TypedConstant c _) = viewConstantRelation c
viewConstantRelation (ConstantFromJSON xs) = mapM viewConstantTuple xs
viewConstantRelation constant = fail ("Expecting a relation, but got:" <++> pretty constant)

viewConstantPartition :: MonadFail m => Constant -> m [[Constant]]
viewConstantPartition (ConstantAbstract (AbsLitPartition xs)) = return xs
viewConstantPartition (TypedConstant c _) = viewConstantPartition c
viewConstantPartition (ConstantFromJSON xs) = mapM viewConstantSet xs
viewConstantPartition constant = fail ("Expecting a partition, but got:" <++> pretty constant)

