{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestUtils where


import Control.Applicative
import Control.Monad ( void )
import System.Random ( StdGen, newStdGen, split )

import Test.HUnit ( Test, (~:), (~=?), runTestTT, test )
import qualified Test.QuickCheck.Arbitrary as QC ( Arbitrary, arbitrary )
import qualified Test.QuickCheck.Gen as QC ( Gen(..) )


-- given a string and a QuickCheck property, constructs a HUnit test
quickTestBool :: forall a . QC.Arbitrary a => String -> (a -> Bool) -> IO [Test]
quickTestBool str f = do
    samples :: [a] <- sampler QC.arbitrary
    return [ msg ~: f s ~=? True
           | (i,s) <- zip [(1::Int)..] samples
           , let msg = "quickTest: " ++ str ++ "[" ++ show i ++ "]"
           ]


-- will use the Arbitrary instance to generate a random value of type a, and generate a sample of tests.
quickTest :: forall a . (QC.Arbitrary a, Show a) => (a -> Test) -> IO [Test]
quickTest f = do
    samples :: [a] <- sampler QC.arbitrary
    -- mapM_ print samples -- uncomment to see the randomly generated values
    return $ map f samples


runTest :: Test -> IO ()
runTest t = void $ runTestTT t


runTestNTimes :: Int -> Test -> IO ()
runTestNTimes i t = print =<< runTestTT (test (replicate i t))


sampler :: QC.Gen a -> IO [a]
sampler (QC.MkGen m) = do
    rs <- randoms
    return [ m r n
           | (r, n) <- zip rs [0,5..100]
           ]
    where
        randoms :: IO [StdGen]
        randoms = go <$> newStdGen
            where
                go r0 = r1 : go r2
                    where (r1,r2) = split r0
