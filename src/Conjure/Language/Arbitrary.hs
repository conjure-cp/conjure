{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Conjure.Language.Arbitrary
    ( AnyDomainTuple(..)
    , AnyConstantTuple(..)
    , AnyDomainAndConstant(..)
    , arbitraryDomainAndConstant
    , sampleArbitraryDomainAndConstant
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Pretty
import Conjure.Language.DomainSize ( domainSizeConstant )

-- QuickCheck
import Test.QuickCheck ( Arbitrary(..), Gen, sized, choose, oneof, vectorOf, sample' )


newtype AnyDomainTuple a = AnyDomainTuple (Domain () a)
    deriving (Show)

instance Arbitrary a => Arbitrary (AnyDomainTuple a) where
    arbitrary = do
        arity <- choose (2 :: Int, 10)
        xs    <- vectorOf arity arbitrary
        return $ AnyDomainTuple $ DomainTuple xs
    shrink (AnyDomainTuple d) =
        case d of
            DomainTuple xs -> 
                [ AnyDomainTuple (DomainTuple ys)
                | ys <- subsequences xs
                , not (null ys)
                , length ys < length xs
                ]
            _ -> []


newtype AnyConstantTuple = AnyConstantTuple Constant
    deriving (Show)

instance Arbitrary AnyConstantTuple where
    arbitrary = do
        arity <- choose (2 :: Int, 10)
        xs    <- vectorOf arity arbitrary
        return $ AnyConstantTuple $ ConstantTuple xs
    shrink (AnyConstantTuple c) =
        case c of
            ConstantTuple xs -> 
                [ AnyConstantTuple (ConstantTuple ys)
                | ys <- subsequences xs
                , not (null ys)
                , length ys < length xs
                ]
            _ -> []


-- | Some of these arbitrary generators are a bit crap.
--   They recursively call themselves if some property doesn't hold in the generated value.
--   This is the number of maximum retries.
maxRetries :: Int
maxRetries = 1000000

data AnyDomainAndConstant = AnyDomainAndConstant (Domain HasRepresentation Constant) Constant

instance Show AnyDomainAndConstant where
    show (AnyDomainAndConstant domain constant) = show $ vcat
        [ "AnyDomainAndConstant"
        , "        domain   :" <+> pretty domain
        , "        constant :" <+> pretty constant
        ]
instance Arbitrary AnyDomainAndConstant where
    arbitrary = do
        (domain, constantGen) <- arbitraryDomainAndConstant
        constant <- constantGen
        return (AnyDomainAndConstant domain constant)

-- | This is a great function!
--   It generates a random domain, and a generator of random constants of that domain.
--   Follow the function calls starting from dispatch to see how it's implemented. It is pretty straightforward really.
--   Note: The nesting level is controlled via the `sized` combinator from QuickCheck.
arbitraryDomainAndConstant :: Gen (Domain HasRepresentation Constant, Gen Constant)
arbitraryDomainAndConstant = sized dispatch

    where

        -- this is how size gets reduced in recursive calls
        smaller :: Int -> Int
        smaller depth = max 0 (div depth 10)

        dispatch :: Int -> Gen (Domain HasRepresentation Constant, Gen Constant)
        dispatch 0 = oneof [bool, int]
        dispatch d = oneof [bool, int, tuple d, matrix d, set d]

        bool :: Gen (Domain HasRepresentation Constant, Gen Constant)
        bool = return (DomainBool, ConstantBool <$> arbitrary)

        int :: Gen (Domain r Constant, Gen Constant)
        int = oneof [intBounded, intSingles, intMixed]

        intBounded :: Gen (Domain r Constant, Gen Constant)
        intBounded = do
            l <- choose (0 :: Int, 100)
            u <- choose (l, 200)
            return ( DomainInt [RangeBounded (ConstantInt l) (ConstantInt u)]
                   , ConstantInt <$> choose (l,u)
                   )

        intSingles :: Gen (Domain r Constant, Gen Constant)
        intSingles = do
            count <- choose (1 :: Int, 20)
            vals  <- vectorOf count (choose (0 :: Int, 100))
            return ( DomainInt (map (RangeSingle . ConstantInt) vals)
                   , ConstantInt <$> pickFromList vals
                   )

        intMixed :: Gen (Domain r Constant, Gen Constant)
        intMixed = do
            let single = RangeSingle . ConstantInt <$> choose (0 :: Int, 100)
            let pair = do l <- choose (0 :: Int, 100)
                          u <- choose (l, 200)
                          return $ RangeBounded (ConstantInt l) (ConstantInt u)

            numSingles <- choose (1 :: Int, 10)
            numPairs <- choose (1 :: Int, 10)

            let
                -- first argument, number of `RangeSingle`s to generate
                -- second argument, number of `RangeBounded`s to generate
                genRanges 0 0 = return []
                genRanges 0 p = vectorOf p pair
                genRanges s 0 = vectorOf s single
                genRanges s p = do
                    which <- arbitrary
                    if which
                        then (:) <$> single <*> genRanges (s-1) p
                        else (:) <$> pair   <*> genRanges s (p-1)

            rs <- genRanges numSingles numPairs

            let allVals = nub $ sort $ concat
                    [ vals
                    | r <- rs
                    , let vals = case r of
                            RangeSingle (ConstantInt i) -> [i]
                            RangeBounded (ConstantInt l) (ConstantInt u) -> [l..u]
                            _ -> []
                    ]

            if null allVals
                then error "allVals null"
                else return ( DomainInt rs
                            , ConstantInt <$> pickFromList allVals
                            )

        -- enum :: Gen (Domain HasRepresentation Constant, Gen Constant)
        -- enum = undefined

        tuple :: Int -> Gen (Domain HasRepresentation Constant, Gen Constant)
        tuple depth = do
            arity <- choose (2 :: Int, 4)
            (ds, cs) <- unzip <$> vectorOf arity (dispatch (smaller depth))
            return ( DomainTuple ds
                   , ConstantTuple <$> sequence cs
                   )

        matrix :: Int -> Gen (Domain HasRepresentation Constant, Gen Constant)
        matrix depth = do
            (indexDomain, _) <- int
            case domainSizeConstant indexDomain of
                Left err -> error $ show err
                Right indexSize -> do
                    (innerDomain, innerConstantGen) <- dispatch (smaller depth)
                    return ( DomainMatrix indexDomain innerDomain
                           , do innerConstants <- vectorOf indexSize innerConstantGen
                                return $ ConstantMatrix indexDomain innerConstants
                           )

        set :: Int -> Gen (Domain HasRepresentation Constant, Gen Constant)
        set depth = oneof [setFixed depth, setBounded depth]

        setFixed :: Int -> Gen (Domain HasRepresentation Constant, Gen Constant)
        setFixed depth = do
            (dom, constantGen) <- dispatch (smaller depth)
            let sizeUpTo = case domainSizeConstant dom of
                                Left err -> error $ show err
                                Right s  -> min 10 s
            size <- choose (0 :: Int, sizeUpTo)
            repr <- pickFromList ["Explicit"] -- no other representation yet!
            let domainOut =
                    DomainSet
                        (HasRepresentation repr)
                        (SetAttrSize (ConstantInt size))
                        dom
            return ( domainOut
                   , let try n =
                            if n >= maxRetries
                                then fail (vcat [ "setFixed: maxRetries"
                                                , pretty domainOut
                                                ])
                                else do
                                    elems <- vectorOf size constantGen
                                    let sorted = sort $ nub elems
                                    if length sorted == length elems
                                        then return (ConstantSet sorted)
                                        else try (n+1)
                     in try (1 :: Int)
                   )

        setBounded :: Int -> Gen (Domain HasRepresentation Constant, Gen Constant)
        setBounded depth = oneof [setBoundedMax depth, setBoundedMinMax depth]

        setBoundedMax :: Int -> Gen (Domain HasRepresentation Constant, Gen Constant)
        setBoundedMax depth = do
            (dom, constantGen) <- dispatch (smaller depth)
            let sizeUpTo = case domainSizeConstant dom of
                                Left err -> error $ show err
                                Right s  -> min 10 s
            maxSize <- choose (0 :: Int, sizeUpTo)
            repr <- pickFromList ["ExplicitVarSizeWithBoolMarkers", "ExplicitVarSizeWithIntMarker" ] -- these representations do not exist yet!
            return ( DomainSet (HasRepresentation repr)
                               (SetAttrMaxSize (ConstantInt maxSize))
                               dom
                   , do numElems <- choose (0, maxSize)
                        elems <- vectorOf numElems constantGen
                        let sorted = sort $ nub elems
                        return (ConstantSet sorted)
                   )

        setBoundedMinMax :: Int -> Gen (Domain HasRepresentation Constant, Gen Constant)
        setBoundedMinMax depth = do
            (dom, constantGen) <- dispatch (smaller depth)
            let sizeUpTo = case domainSizeConstant dom of
                                Left err -> error $ show err
                                Right s  -> min 10 s
            minSize <- choose (0 :: Int, sizeUpTo)
            maxSize <- choose (minSize, sizeUpTo)
            repr <- pickFromList ["ExplicitVarSizeWithBoolMarkers", "ExplicitVarSizeWithIntMarker" ] -- these representations do not exist yet!
            let domainOut =
                    DomainSet
                        (HasRepresentation repr)
                        (SetAttrMinMaxSize
                            (ConstantInt minSize)
                            (ConstantInt maxSize))
                        dom
            return ( domainOut
                   , let try n =
                            if n >= maxRetries
                                then fail (vcat [ "setFixed: maxRetries"
                                                , pretty domainOut
                                                ])
                                else do
                                    numElems <- choose (minSize, maxSize)
                                    elems <- vectorOf numElems constantGen
                                    let sorted = sort $ nub elems
                                    if length sorted >= minSize
                                        then return (ConstantSet sorted)
                                        else try (n+1)
                     in try (1 :: Int)
                   )

pickFromList :: [a] -> Gen a
pickFromList [] = fail "pickFromList []"
pickFromList xs = do
    index <- choose (0, length xs - 1)
    return (xs `at` index)

sampleArbitraryDomainAndConstant :: IO ()
sampleArbitraryDomainAndConstant = do
    samples <- sample' arbitraryDomainAndConstant
    forM_ samples $ \ (dom, consGen) -> do
        print $ "domain          :" <+> pretty dom
        constants <- sample' consGen
        print $ "constants       :" <+> vcat (map (("-" <+>) . pretty) constants)


