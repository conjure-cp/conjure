{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Conjure.Language.Arbitrary
    ( AnyDomainTuple(..)
    , AnyConstantTuple(..)
    , arbitraryDomainAndConstant
    , sampleArbitraryDomainAndConstant
    ) where

-- conjure
import Conjure.Language.Definition
import Conjure.Language.Pretty
import Conjure.Language.DomainSize ( domainSizeConstant )

-- base
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad ( forM_ )
import Data.List ( nub, sort, subsequences )

-- QuickCheck
import Test.QuickCheck ( Arbitrary(..), Gen, sized, choose, oneof, vectorOf, sample' )

-- safe
import Safe ( at )

-- pretty
import Text.PrettyPrint ( vcat )


newtype AnyDomainTuple a = AnyDomainTuple (Domain a)
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
                , length ys >= 1
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
                , length ys >= 1
                , length ys < length xs
                ]
            _ -> []


-- | Some of these arbitrary generators are a bit crap.
--   They recursively call themselves if some property doesn't hold in the generated value.
--   This is the number of maximum retries.
maxRetries :: Int
maxRetries = 10000

-- | This is a great function!
--   It generates a random domain, and a generator of random constants of that domain.
--   Follow the function calls starting from dispatch to see how it's implemented. It is pretty straightforward really.
--   Note: The nesting level is controlled via the `sized` combinator from QuickCheck.
arbitraryDomainAndConstant :: Gen (Domain Constant, Gen Constant)
arbitraryDomainAndConstant = sized dispatch

    where

        -- this is how size gets reduced in recursive calls
        smaller depth = max 0 (div depth 10)

        dispatch :: Int -> Gen (Domain Constant, Gen Constant)
        dispatch 0 = oneof [bool, int]
        dispatch d = oneof [tuple d, set d]

        bool :: Gen (Domain Constant, Gen Constant)
        bool = return (DomainBool, ConstantBool <$> arbitrary)

        int :: Gen (Domain Constant, Gen Constant)
        int = oneof [intBounded, intSingles, intMixed]

        intBounded :: Gen (Domain Constant, Gen Constant)
        intBounded = do
            l <- choose (0 :: Int, 100)
            u <- choose (l, 200)
            return ( DomainInt [RangeBounded (ConstantInt l) (ConstantInt u)]
                   , ConstantInt <$> choose (l,u)
                   )

        intSingles :: Gen (Domain Constant, Gen Constant)
        intSingles = do
            count <- choose (1 :: Int, 20)
            vals  <- vectorOf count (choose (0 :: Int, 100))
            return ( DomainInt (map (RangeSingle . ConstantInt) vals)
                   , ConstantInt <$> pickFromList (head vals) vals
                   )

        intMixed :: Gen (Domain Constant, Gen Constant)
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
                            , ConstantInt <$> pickFromList (head allVals) allVals
                            )

        -- enum :: Gen (Domain Constant, Gen Constant)
        -- enum = undefined

        tuple :: Int -> Gen (Domain Constant, Gen Constant)
        tuple depth = do
            arity <- choose (2 :: Int, 4)
            (ds, cs) <- unzip <$> vectorOf arity (dispatch (smaller depth))
            return ( DomainTuple ds
                   , ConstantTuple <$> sequence cs
                   )

        set :: Int -> Gen (Domain Constant, Gen Constant)
        set depth = oneof [setFixed depth, setBounded depth]

        setFixed :: Int -> Gen (Domain Constant, Gen Constant)
        setFixed depth = do
            (dom, constantGen) <- dispatch (smaller depth)
            let sizeUpTo = case domainSizeConstant dom of
                                Nothing -> error $ show $ "This domain seems to be infinite:" <+> pretty dom
                                Just s  -> min 10 s
            size <- choose (2 :: Int, sizeUpTo)
            return ( DomainSet (DomainAttributes [DANameValue "size" (ConstantInt size)]) dom
                   , let try n = if n >= maxRetries then fail "setBoundedMinMax: maxRetries" else do
                           elems <- vectorOf size constantGen
                           let sorted = sort $ nub elems
                           if length sorted == length elems
                               then return (ConstantSet sorted)
                               else try (n+1)
                     in try (1 :: Int)
                   )

        setBounded :: Int -> Gen (Domain Constant, Gen Constant)
        setBounded depth = oneof [setBoundedMax depth, setBoundedMinMax depth]

        setBoundedMax :: Int -> Gen (Domain Constant, Gen Constant)
        setBoundedMax depth = do
            (dom, constantGen) <- dispatch (smaller depth)
            let sizeUpTo = case domainSizeConstant dom of
                                Nothing -> error $ show $ "This domain seems to be infinite:" <+> pretty dom
                                Just s  -> min 10 s
            maxSize <- choose (2 :: Int, sizeUpTo)
            return ( DomainSet (DomainAttributes [DANameValue "maxSize" (ConstantInt maxSize)]) dom
                   , do numElems <- choose (0, maxSize)
                        elems <- vectorOf numElems constantGen
                        let sorted = sort $ nub elems
                        return (ConstantSet sorted)
                   )

        setBoundedMinMax :: Int -> Gen (Domain Constant, Gen Constant)
        setBoundedMinMax depth = do
            (dom, constantGen) <- dispatch (smaller depth)
            let sizeUpTo = case domainSizeConstant dom of
                                Nothing -> error $ show $ "This domain seems to be infinite:" <+> pretty dom
                                Just s  -> min 10 s
            minSize <- choose (2 :: Int, sizeUpTo)
            maxSize <- choose (minSize, sizeUpTo)
            return ( DomainSet (DomainAttributes [ DANameValue "minSize" (ConstantInt minSize)
                                                 , DANameValue "maxSize" (ConstantInt maxSize)
                                                 ]) dom
                   , let try n = if n >= maxRetries then fail "setBoundedMinMax: maxRetries" else do
                           numElems <- choose (minSize, maxSize)
                           elems <- vectorOf numElems constantGen
                           let sorted = sort $ nub elems
                           if length sorted >= minSize
                               then return (ConstantSet sorted)
                               else try (n+1)
                     in try (1 :: Int)
                   )

pickFromList :: a -> [a] -> Gen a
pickFromList x [] = return x
pickFromList _ xs = do
    index <- choose (0, length xs - 1)
    return (xs `at` index)

sampleArbitraryDomainAndConstant :: IO ()
sampleArbitraryDomainAndConstant = do
    samples <- sample' arbitraryDomainAndConstant
    forM_ samples $ \ (dom, consgen) -> do
        print $ "domain:" <+> pretty dom
        constants <- sample' consgen
        print $ "constants:" <+> vcat (map (("-" <+>) . pretty) constants)


