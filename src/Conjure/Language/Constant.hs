{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.Language.Constant
    ( Constant(..)
    , valuesInIntDomain
    , normaliseConstant
    , validateConstantForDomain
    , mkUndef, isUndef
    , emptyCollection
    , viewConstantBool
    , viewConstantInt
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

instance Arbitrary Constant where
    arbitrary = oneof
        [ ConstantBool <$> arbitrary
        , ConstantInt <$> arbitrary
        ]

instance TypeOf Constant where
    typeOf ConstantBool{}             = return TypeBool
    typeOf ConstantInt{}              = return TypeInt
    typeOf (ConstantEnum defn _ _ )   = return (TypeEnum defn)
    typeOf (ConstantField _ ty)       = return ty
    typeOf (ConstantAbstract x    )   = typeOf x
    typeOf (DomainInConstant dom)     = typeOf dom
    typeOf (TypedConstant _ ty)       = return ty
    typeOf (ConstantUndefined _ ty)   = return ty

instance DomainSizeOf Constant Integer where
    domainSizeOf DomainBool{} = return 2
    domainSizeOf (DomainIntE x) = bug ("not implemented, domainSizeOf DomainIntE" <+> pretty (show x))
    domainSizeOf (DomainInt rs) = domainSizeOfRanges rs
    domainSizeOf DomainEnum{} = fail "domainSizeOf: Unknown for given enum."
    domainSizeOf (DomainTuple ds) = product <$> mapM domainSizeOf ds
    domainSizeOf (DomainMatrix index inner) = intPow <$> domainSizeOf inner <*> domainSizeOf index
    domainSizeOf d@(DomainSet _ (SetAttr attrs) inner) =
        case attrs of
            SizeAttr_None -> do
                innerSize <- domainSizeOf inner
                return (2 `intPow` innerSize)
            SizeAttr_Size (ConstantInt size) -> do
                innerSize <- domainSizeOf inner
                return (nchoosek (product . enumFromTo 1) innerSize size)
            SizeAttr_MinSize{} -> do
                -- TODO: we can do better here
                innerSize <- domainSizeOf inner
                return (2 `intPow` innerSize)
            SizeAttr_MaxSize (ConstantInt maxSize) -> do
                innerSize <- domainSizeOf inner
                return $ sum [ nchoosek (product . enumFromTo 1) innerSize k | k <- [0 .. maxSize] ]
            SizeAttr_MinMaxSize (ConstantInt minSize) (ConstantInt maxSize) -> do
                innerSize <- domainSizeOf inner
                return $ sum [ nchoosek (product . enumFromTo 1) innerSize k | k <- [minSize .. maxSize] ]
            _ -> fail ("domainSizeOf{Constant}" <+> pretty d)
    domainSizeOf DomainMSet              {} = bug "not implemented: domainSizeOf DomainMSet"
    domainSizeOf DomainFunction          {} = bug "not implemented: domainSizeOf DomainFunction"
    domainSizeOf DomainRelation          {} = bug "not implemented: domainSizeOf DomainRelation"
    domainSizeOf DomainPartition         {} = bug "not implemented: domainSizeOf DomainPartition"
    domainSizeOf _                          = bug "not implemented: domainSizeOf"

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
domainSizeOfRanges = fmap genericLength . valuesInIntDomain

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
        else fail $ "Expected finite integer ranges, but got:" <++> prettyList id "," ranges

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
validateConstantForDomain :: forall m r . (MonadFail m, Pretty r) => Name -> Constant -> Domain r Constant -> m ()

validateConstantForDomain _ ConstantBool{} DomainBool{} = return ()

validateConstantForDomain _ _ (DomainInt []) = return ()              -- no restrictions

validateConstantForDomain name c@(ConstantInt i) d@(DomainInt rs) =
    let
        intInRange RangeOpen                                      = True
        intInRange (RangeSingle (ConstantInt a))                  = i == a
        intInRange (RangeLowerBounded (ConstantInt a))            = i >= a
        intInRange (RangeUpperBounded (ConstantInt a))            = i <= a
        intInRange (RangeBounded (ConstantInt a) (ConstantInt b)) = i >= a && i <= b
        intInRange _                                              = False
    in  unless (any intInRange rs) (constantNotInDomain name c d)

validateConstantForDomain _ (ConstantInt i) (DomainUnnamed _ (ConstantInt a)) | i >= 1 && i <= a = return ()

validateConstantForDomain _ _ (DomainEnum _ Nothing _) = return ()    -- no restrictions
validateConstantForDomain name c d@(DomainEnum _ _ Nothing) =
    fail $ vcat [ "validateConstantForDomain: enum not handled"
                , pretty name
                , pretty c
                , pretty d
                ]
validateConstantForDomain name
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
        validateConstantForDomain name c (DomainInt rs :: Domain r Constant)

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitTuple cs))
    d@(DomainTuple ds) = nested c d $ zipWithM_ (validateConstantForDomain name) cs ds

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitRecord (sortOn fst -> cs)))
    d@(DomainRecord (sortOn fst -> ds))
        | map fst cs == map fst ds
            = nested c d $ zipWithM_ (validateConstantForDomain name) (map snd cs) (map snd ds)
        | otherwise
            = constantNotInDomain name c d

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitVariant _ n c'))
    d@(DomainVariant ds)
        | Just d' <- lookup n ds
            = nested c d $ validateConstantForDomain name c' d'
        | otherwise
            = constantNotInDomain name c d

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitMatrix cIndex vals))
    d@(DomainMatrix dIndex dInner) = do
        nested c d $
            mapM_ (\ val -> validateConstantForDomain name val dInner ) vals
        unless (cIndex == dIndex || cIndex == DomainInt []) $ fail $ vcat
            [ "The indices do not match between the value and the domain."
            , "Value :" <+> pretty c
            , "Domain:" <+> pretty d
            ]

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitSet vals))
    d@(DomainSet _ (SetAttr sizeAttr) dInner) = do
        let cardinalityOK = case sizeAttr of
                SizeAttr_None -> True
                SizeAttr_Size (ConstantInt s) -> s == genericLength vals
                SizeAttr_MinSize (ConstantInt s) -> s <= genericLength vals
                SizeAttr_MaxSize (ConstantInt s) -> genericLength vals <= s
                SizeAttr_MinMaxSize (ConstantInt smin) (ConstantInt smax) ->
                    smin <= genericLength vals && genericLength vals <= smax
                _ -> False
        unless cardinalityOK $ fail $ vcat
            [ "The value is not a member of the domain."
            , "Value :" <+> pretty c
            , "Domain:" <+> pretty d
            , "Reason: Domain attributes are not satisfied."
            , "Specifically:" <+> pretty sizeAttr
            ]
        nested c d $ mapM_ (\ val -> validateConstantForDomain name val dInner ) vals

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitMSet vals))
    d@(DomainMSet _ (MSetAttr sizeAttr occurAttr) dInner) = do
        let cardinalityOK = case sizeAttr of
                SizeAttr_None -> True
                SizeAttr_Size (ConstantInt s) -> s == genericLength vals
                SizeAttr_MinSize (ConstantInt s) -> s <= genericLength vals
                SizeAttr_MaxSize (ConstantInt s) -> genericLength vals <= s
                SizeAttr_MinMaxSize (ConstantInt smin) (ConstantInt smax) ->
                    smin <= genericLength vals && genericLength vals <= smax
                _ -> False
        unless cardinalityOK $ fail $ vcat
            [ "The value is not a member of the domain."
            , "Value :" <+> pretty c
            , "Domain:" <+> pretty d
            , "Reason: Domain attributes are not satisfied."
            , "Specifically:" <+> pretty sizeAttr
            ]
        let occurOK = case occurAttr of
                OccurAttr_None -> True
                OccurAttr_MinOccur (ConstantInt s) -> and [ s <= occ | (_, occ) <- histogram vals ]
                OccurAttr_MaxOccur (ConstantInt s) -> and [ occ <= s | (_, occ) <- histogram vals ]
                OccurAttr_MinMaxOccur (ConstantInt smin) (ConstantInt smax) ->
                    and [ smin <= occ && occ <= smax | (_, occ) <- histogram vals ]
                _ -> False
        unless occurOK $ fail $ vcat
            [ "The value is not a member of the domain."
            , "Value :" <+> pretty c
            , "Domain:" <+> pretty d
            , "Reason: Domain attributes are not satisfied."
            , "Specifically:" <+> pretty occurAttr
            ]
        nested c d $ mapM_ (\ val -> validateConstantForDomain name val dInner ) vals

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitFunction vals))
    d@(DomainFunction _ _ dFrom dTo) = nested c d $ do
        mapM_ (\ val -> validateConstantForDomain name (fst val) dFrom) vals
        mapM_ (\ val -> validateConstantForDomain name (snd val) dTo  ) vals

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitSequence vals))
    d@(DomainSequence _ _ dInner) = nested c d $
        mapM_ (\ val -> validateConstantForDomain name val dInner ) vals

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitRelation valss))
    d@(DomainRelation _ _ dInners) = nested c d $
        forM_ valss $ \ vals ->
            zipWithM_ (validateConstantForDomain name) vals dInners

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitPartition valss))
    d@(DomainPartition _ _ dInner) = nested c d $
        mapM_ (\ val -> validateConstantForDomain name val dInner ) (concat valss)

validateConstantForDomain name c@(TypedConstant c' _) d = nested c d $ validateConstantForDomain name c' d

validateConstantForDomain name c d = constantNotInDomain name c d


nested :: (MonadFail m, Pretty r) => Constant -> Domain r Constant -> Either Doc () -> m ()
nested _ _ Right{} = return ()
nested c d (Left err) = fail $ vcat
    [ "The value is not a member of the domain."
    , "Value :" <+> pretty c
    , "Domain:" <+> pretty d
    , "Reason:"
    , nest 4 err
    ]

constantNotInDomain :: (MonadFail m, Pretty r) => Name -> Constant -> Domain r Constant -> m ()
constantNotInDomain n c d = fail $ vcat
    [ "The value is not a member of the domain."
    , "Name  :" <+> pretty n
    , "Value :" <+> pretty c
    , "Domain:" <+> pretty d
    ]


viewConstantBool      :: MonadFail m => Constant -> m Bool
viewConstantBool      (ConstantBool i) = return i
viewConstantBool      (ConstantInt  0) = return False
viewConstantBool      (ConstantInt  1) = return True
viewConstantBool      constant = fail ("Expecting a boolean integer, but got:" <++> pretty constant)

viewConstantInt       :: MonadFail m => Constant -> m Integer
viewConstantInt       (ConstantInt i) = return i
viewConstantInt       constant = fail ("Expecting an integer, but got:" <++> pretty constant)

viewConstantTuple     :: MonadFail m => Constant -> m [Constant]
viewConstantTuple     (ConstantAbstract (AbsLitTuple xs)) = return xs
viewConstantTuple     (TypedConstant c _) = viewConstantTuple c
viewConstantTuple    constant = fail ("Expecting a tuple, but got:" <++> pretty constant)

viewConstantRecord    :: MonadFail m => Constant -> m [(Name, Constant)]
viewConstantRecord    (ConstantAbstract (AbsLitRecord xs)) = return xs
viewConstantRecord    (TypedConstant c _) = viewConstantRecord c
viewConstantRecord    constant = fail ("Expecting a record, but got:" <++> pretty constant)

viewConstantVariant   :: MonadFail m => Constant -> m (Maybe [(Name, Domain () Constant)], Name, Constant)
viewConstantVariant   (ConstantAbstract (AbsLitVariant lu nm x)) = return (lu, nm, x)
viewConstantVariant   (TypedConstant c _) = viewConstantVariant c
viewConstantVariant   constant = fail ("Expecting a variant, but got:" <++> pretty constant)

viewConstantMatrix    :: MonadFail m => Constant -> m (Domain () Constant, [Constant])
viewConstantMatrix    (ConstantAbstract (AbsLitMatrix ind xs)) = return (ind, xs)
viewConstantMatrix    (TypedConstant c _) = viewConstantMatrix c
viewConstantMatrix    constant = fail ("Expecting a matrix, but got:" <++> pretty constant)

viewConstantSet       :: MonadFail m => Constant -> m [Constant]
viewConstantSet       (ConstantAbstract (AbsLitSet xs)) = return xs
viewConstantSet       (TypedConstant c _) = viewConstantSet c
viewConstantSet       constant = fail ("Expecting a set, but got:" <++> pretty constant)

viewConstantMSet      :: MonadFail m => Constant -> m [Constant]
viewConstantMSet      (ConstantAbstract (AbsLitMSet xs)) = return xs
viewConstantMSet      (TypedConstant c _) = viewConstantMSet c
viewConstantMSet      constant = fail ("Expecting an mset, but got:" <++> pretty constant)

viewConstantFunction  :: MonadFail m => Constant -> m [(Constant, Constant)]
viewConstantFunction  (ConstantAbstract (AbsLitFunction xs)) = return xs
viewConstantFunction  (TypedConstant c _) = viewConstantFunction c
viewConstantFunction  constant = do
    let
        suggestion = case constant of
            ConstantAbstract (AbsLitMatrix (DomainInt rs) vals) -> do
                froms <- valuesInIntDomain rs
                return $ Just $ pretty $ AbsLitFunction (zip (map ConstantInt froms) vals)
            _ -> return Nothing
    suggestion >>= \case
        Nothing  -> fail ("Expecting a function, but got:" <++> pretty constant)
        Just sug -> fail (vcat [ "Expecting a function, but got:" <++> pretty constant
                               , "Maybe you meant:" <++> sug
                               ])

viewConstantSequence  :: MonadFail m => Constant -> m [Constant]
viewConstantSequence  (ConstantAbstract (AbsLitSequence xs)) = return xs
viewConstantSequence  (TypedConstant c _) = viewConstantSequence c
viewConstantSequence  constant = fail ("Expecting a sequence, but got:" <++> pretty constant)

viewConstantRelation  :: MonadFail m => Constant -> m [[Constant]]
viewConstantRelation  (ConstantAbstract (AbsLitRelation xs)) = return xs
viewConstantRelation  (TypedConstant c _) = viewConstantRelation c
viewConstantRelation  constant = fail ("Expecting a relation, but got:" <++> pretty constant)

viewConstantPartition :: MonadFail m => Constant -> m [[Constant]]
viewConstantPartition (ConstantAbstract (AbsLitPartition xs)) = return xs
viewConstantPartition (TypedConstant c _) = viewConstantPartition c
viewConstantPartition constant = fail ("Expecting a partition, but got:" <++> pretty constant)
