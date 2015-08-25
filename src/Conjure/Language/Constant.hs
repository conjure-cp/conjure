{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Conjure.Language.Constant
    ( Constant(..)
    , valuesInIntDomain
    , normaliseConstant
    , validateConstantForDomain
    , mkUndef, isUndef
    , emptyCollection
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


data Constant
    = ConstantBool Bool
    | ConstantInt Integer
    | ConstantEnum Name   {- name for the enum domain -}
                   [Name] {- values in the enum domain -}
                   Name   {- the literal -}
    | ConstantField Name Type                               -- the name of a field of Record or Variant and its type
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
    compare (ConstantInt a) (ConstantInt b) = compare a b
    compare (ConstantEnum a1 a2 a3) (ConstantEnum b1 b2 b3) = compare (a1,a2,a3) (b1,b2,b3)
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

instance Arbitrary Constant where
    arbitrary = oneof
        [ ConstantBool <$> arbitrary
        , ConstantInt <$> arbitrary
        ]

instance TypeOf Constant where
    typeOf ConstantBool{}             = return TypeBool
    typeOf ConstantInt{}              = return TypeInt
    typeOf (ConstantEnum defn _ _ )   = return (TypeEnum defn)
    typeOf (ConstantField _ ty) = return ty
    typeOf (ConstantAbstract x    )   = typeOf x
    typeOf (DomainInConstant dom)     = typeOf dom
    typeOf (TypedConstant _ ty)       = return ty
    typeOf (ConstantUndefined _ ty)   = return ty

instance DomainSizeOf Constant Integer where
    domainSizeOf DomainBool{} = return 2
    domainSizeOf (DomainInt rs) = domainSizeOfRanges rs
    domainSizeOf DomainEnum{} = fail "domainSizeOf: Unknown for given enum."
    domainSizeOf (DomainTuple ds) = product <$> mapM domainSizeOf ds
    domainSizeOf (DomainMatrix index inner) = intPow <$> domainSizeOf inner <*> domainSizeOf index
    domainSizeOf (DomainSet _ (SetAttr attrs) inner) =
        case attrs of
            SizeAttr_None -> do
                innerSize <- domainSizeOf inner
                return (2 `intPow` innerSize)
            SizeAttr_Size (ConstantInt size) -> do
                innerSize <- domainSizeOf inner
                return (nchoosek (product . enumFromTo 1) innerSize size)
            SizeAttr_MaxSize (ConstantInt maxSize) -> do
                innerSize <- domainSizeOf inner
                return $ sum [ nchoosek (product . enumFromTo 1) innerSize k | k <- [0 .. maxSize] ]
            SizeAttr_MinMaxSize (ConstantInt minSize) (ConstantInt maxSize) -> do
                innerSize <- domainSizeOf inner
                return $ sum [ nchoosek (product . enumFromTo 1) innerSize k | k <- [minSize .. maxSize] ]
            _ -> fail "domainSizeOf"
    domainSizeOf (DomainMSet      {}) = bug "not implemented: domainSizeOf DomainMSet"
    domainSizeOf (DomainFunction  {}) = bug "not implemented: domainSizeOf DomainFunction"
    domainSizeOf (DomainRelation  {}) = bug "not implemented: domainSizeOf DomainRelation"
    domainSizeOf (DomainPartition {}) = bug "not implemented: domainSizeOf DomainPartition"
    domainSizeOf _                    = bug "not implemented: domainSizeOf"

emptyCollection :: Constant -> Bool
emptyCollection ConstantBool{} = False
emptyCollection ConstantInt{} = False
emptyCollection ConstantEnum{} = False
emptyCollection ConstantField{} = False
emptyCollection (ConstantAbstract x) = emptyCollectionAbsLit x
emptyCollection DomainInConstant{} = False
emptyCollection (TypedConstant x _) = emptyCollection x
emptyCollection ConstantUndefined{} = False

intPow :: Integer -> Integer -> Integer
intPow = (^)

domainSizeOfRanges :: MonadFail m => [Range Constant] -> m Integer
domainSizeOfRanges = liftM genericLength . valuesInIntDomain

instance DomainSizeOf Constant Constant where
    domainSizeOf = fmap ConstantInt . domainSizeOf

instance Pretty Constant where

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
            pretty' TypeInt = "int()"
            pretty' t = pretty t
        in
            prParens $ "[] : `" <> pretty' ty <> "`"

    pretty (ConstantBool False)          = "false"
    pretty (ConstantBool True )          = "true"
    pretty (ConstantInt  x    )          = pretty x
    pretty (ConstantEnum _ _ x)          = pretty x
    pretty (ConstantField n _)           = pretty n
    pretty (ConstantAbstract x)          = pretty x
    pretty (DomainInConstant d)          = "`" <> pretty d <> "`"
    pretty (TypedConstant x ty)          = prParens $ pretty x <+> ":" <+> "`" <> pretty ty <> "`"
    pretty (ConstantUndefined reason ty) = "undefined" <> prParens (pretty reason <+> ":" <+> "`" <> pretty ty <> "`")

instance ExpressionLike Constant where
    fromInt = ConstantInt
    intOut _ (ConstantInt x) = return x
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

instance Num Constant where
    ConstantInt x + ConstantInt y = ConstantInt (x+y)
    x + y = bug $ vcat [ "Num Constant (+)", "x:" <+> pretty x, "y:" <+> pretty y ]
    ConstantInt x - ConstantInt y = ConstantInt (x-y)
    x - y = bug $ vcat [ "Num Constant (-)", "x:" <+> pretty x, "y:" <+> pretty y ]
    ConstantInt x * ConstantInt y = ConstantInt (x*y)
    x * y = bug $ vcat [ "Num Constant (*)", "x:" <+> pretty x, "y:" <+> pretty y ]
    abs (ConstantInt x) = ConstantInt (abs x)
    abs x = bug $ vcat [ "Num Constant abs", "x:" <+> pretty x ]
    signum (ConstantInt x) = ConstantInt (signum x)
    signum x = bug $ vcat [ "Num Constant signum", "x:" <+> pretty x ]
    fromInteger = ConstantInt . fromInteger


valuesInIntDomain :: MonadFail m => [Range Constant] -> m [Integer]
valuesInIntDomain ranges =
    if isFinite
        then return allValues
        else fail $ "Expected finite integer ranges, but got:" <+> prettyList id "," ranges

    where

        allRanges :: [Maybe [Integer]]
        allRanges =
            [ vals
            | r <- ranges
            , let vals = case r of
                    RangeSingle (ConstantInt x) -> return [x]
                    RangeBounded (ConstantInt l) (ConstantInt u) -> return [l..u]
                    _ -> Nothing
            ]

        isFinite :: Bool
        isFinite = Nothing `notElem` allRanges

        allValues :: [Integer]
        allValues = nub $ concat $ catMaybes allRanges


-- | Assuming both the value and the domain are normalised
validateConstantForDomain :: forall m r . (MonadFail m, Pretty r) => Constant -> Domain r Constant -> m ()

validateConstantForDomain ConstantBool{} DomainBool{} = return ()

validateConstantForDomain _ (DomainInt []) = return ()              -- no restrictions

validateConstantForDomain c@(ConstantInt i) d@(DomainInt rs) =
    let
        intInRange RangeOpen                                      = True
        intInRange (RangeSingle (ConstantInt a))                  = i == a
        intInRange (RangeLowerBounded (ConstantInt a))            = i >= a
        intInRange (RangeUpperBounded (ConstantInt a))            = i <= a
        intInRange (RangeBounded (ConstantInt a) (ConstantInt b)) = i >= a && i <= b
        intInRange _                                              = False
    in  unless (any intInRange rs) (constantNotInDomain c d)

validateConstantForDomain _ (DomainEnum _ Nothing _) = return ()    -- no restrictions
validateConstantForDomain c d@(DomainEnum _ _ Nothing) = fail $ vcat [ "validateConstantForDomain: enum not handled"
                                                                     , pretty c
                                                                     , pretty d
                                                                     ]
validateConstantForDomain
    c@ConstantInt{}
    d@(DomainEnum _ (Just ranges) (Just mp)) = nested c d $ do
        let
            -- lu :: MonadFail m => Name -> m Constant
            lu (ConstantEnum _ _ nm) =
                case lookup nm mp of
                    Nothing -> fail $ "No value for:" <+> pretty nm
                    Just v  -> return (ConstantInt v)
            lu (ConstantInt v) = return (ConstantInt v)
            lu x = fail $ "validateConstantForDomain.lu" <+> pretty x

            -- lu2 :: MonadFail m => Range Name -> m (Range Constant)
            lu2 = mapM lu

        rs <- mapM lu2 ranges
        validateConstantForDomain c (DomainInt rs :: Domain r Constant)

validateConstantForDomain
    c@(ConstantAbstract (AbsLitTuple cs))
    d@(DomainTuple ds) = nested c d $ zipWithM_ validateConstantForDomain cs ds

validateConstantForDomain
    c@(ConstantAbstract (AbsLitRecord cs))
    d@(DomainRecord ds)
        | map fst cs == map fst ds
            = nested c d $ zipWithM_ validateConstantForDomain (map snd cs) (map snd ds)
        | otherwise
            = constantNotInDomain c d

validateConstantForDomain
    c@(ConstantAbstract (AbsLitVariant _ n c'))
    d@(DomainVariant ds)
        | Just d' <- lookup n ds
            = nested c d $ validateConstantForDomain c' d'
        | otherwise
            = constantNotInDomain c d

validateConstantForDomain
    c@(ConstantAbstract (AbsLitMatrix cIndex vals))
    d@(DomainMatrix dIndex dInner) = do
        nested c d $
            mapM_ (`validateConstantForDomain` dInner) vals
        unless (cIndex == dIndex) $ fail $ vcat
            [ "The indices do not match between the value and the domain."
            , "Value :" <+> pretty c
            , "Domain:" <+> pretty d
            ]

validateConstantForDomain
    c@(ConstantAbstract (AbsLitSet vals))
    d@(DomainSet _ _ dInner) = nested c d $
        mapM_ (`validateConstantForDomain` dInner) vals

validateConstantForDomain
    c@(ConstantAbstract (AbsLitMSet vals))
    d@(DomainMSet _ _ dInner) = nested c d $
        mapM_ (`validateConstantForDomain` dInner) vals

validateConstantForDomain
    c@(ConstantAbstract (AbsLitFunction vals))
    d@(DomainFunction _ _ dFrom dTo) = nested c d $ do
        mapM_ (flip validateConstantForDomain dFrom . fst) vals
        mapM_ (flip validateConstantForDomain dTo   . snd) vals

validateConstantForDomain
    c@(ConstantAbstract (AbsLitSequence vals))
    d@(DomainSequence _ _ dInner) = nested c d $
        mapM_ (`validateConstantForDomain` dInner) vals

validateConstantForDomain
    c@(ConstantAbstract (AbsLitRelation valss))
    d@(DomainRelation _ _ dInners) = nested c d $
        forM_ valss $ \ vals ->
            zipWithM_ validateConstantForDomain vals dInners

validateConstantForDomain
    c@(ConstantAbstract (AbsLitPartition valss))
    d@(DomainPartition _ _ dInner) = nested c d $
        mapM_ (`validateConstantForDomain` dInner) (concat valss)

validateConstantForDomain c@(TypedConstant c' _) d = nested c d $ validateConstantForDomain c' d

validateConstantForDomain c d = constantNotInDomain c d


nested :: (MonadFail m, Pretty r) => Constant -> Domain r Constant -> Either Doc () -> m ()
nested _ _ Right{} = return ()
nested c d (Left err) = fail $ vcat
    [ "The value is not a member of the domain."
    , "Value :" <+> pretty c
    , "Domain:" <+> pretty (show d)
    , "Because of:", nest 4 err
    ]

constantNotInDomain :: (MonadFail m, Pretty r) => Constant -> Domain r Constant -> m ()
constantNotInDomain c d = fail $ vcat
    [ "The value is not a member of the domain."
    , "Value :" <+> pretty c
    , "Domain:" <+> pretty (show d)
    ]


viewConstantTuple     :: MonadFail m => Constant -> m [Constant]
viewConstantTuple     (ConstantAbstract (AbsLitTuple xs)) = return xs
viewConstantTuple     (TypedConstant c _) = viewConstantTuple c
viewConstantTuple     _ = fail "viewConstantTuple"

viewConstantRecord    :: MonadFail m => Constant -> m [(Name, Constant)]
viewConstantRecord    (ConstantAbstract (AbsLitRecord xs)) = return xs
viewConstantRecord    (TypedConstant c _) = viewConstantRecord c
viewConstantRecord    _ = fail "viewConstantRecord"

viewConstantVariant   :: MonadFail m => Constant -> m (Maybe [(Name, Domain () Constant)], Name, Constant)
viewConstantVariant   (ConstantAbstract (AbsLitVariant lu nm x)) = return (lu, nm, x)
viewConstantVariant   (TypedConstant c _) = viewConstantVariant c
viewConstantVariant   _ = fail "viewConstantVariant"

viewConstantMatrix    :: MonadFail m => Constant -> m (Domain () Constant, [Constant])
viewConstantMatrix    (ConstantAbstract (AbsLitMatrix ind xs)) = return (ind, xs)
viewConstantMatrix    (TypedConstant c _) = viewConstantMatrix c
viewConstantMatrix    _ = fail "viewConstantMatrix"

viewConstantSet       :: MonadFail m => Constant -> m [Constant]
viewConstantSet       (ConstantAbstract (AbsLitSet xs)) = return xs
viewConstantSet       (TypedConstant c _) = viewConstantSet c
viewConstantSet       _ = fail "viewConstantSet"

viewConstantMSet      :: MonadFail m => Constant -> m [Constant]
viewConstantMSet      (ConstantAbstract (AbsLitMSet xs)) = return xs
viewConstantMSet      (TypedConstant c _) = viewConstantMSet c
viewConstantMSet      _ = fail "viewConstantMSet"

viewConstantFunction  :: MonadFail m => Constant -> m [(Constant, Constant)]
viewConstantFunction  (ConstantAbstract (AbsLitFunction xs)) = return xs
viewConstantFunction  (TypedConstant c _) = viewConstantFunction c
viewConstantFunction  _ = fail "viewConstantFunction"

viewConstantSequence  :: MonadFail m => Constant -> m [Constant]
viewConstantSequence  (ConstantAbstract (AbsLitSequence xs)) = return xs
viewConstantSequence  (TypedConstant c _) = viewConstantSequence c
viewConstantSequence  _ = fail "viewConstantSequence"

viewConstantRelation  :: MonadFail m => Constant -> m [[Constant]]
viewConstantRelation  (ConstantAbstract (AbsLitRelation xs)) = return xs
viewConstantRelation  (TypedConstant c _) = viewConstantRelation c
viewConstantRelation  _ = fail "viewConstantRelation"

viewConstantPartition :: MonadFail m => Constant -> m [[Constant]]
viewConstantPartition (ConstantAbstract (AbsLitPartition xs)) = return xs
viewConstantPartition (TypedConstant c _) = viewConstantPartition c
viewConstantPartition _ = fail "viewConstantPartition"

