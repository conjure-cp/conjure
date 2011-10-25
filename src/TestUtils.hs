{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestUtils where


import Test.HUnit ( Test, (~:), (~=?), runTestTT, test )
import qualified Test.QuickCheck.Arbitrary as QC ( Arbitrary, arbitrary )
import qualified Test.QuickCheck.Gen as QC ( sample' )


-- given a string and a QuickCheck property, constructs a HUnit test
quickTestBool :: forall a . QC.Arbitrary a => String -> (a -> Bool) -> IO [Test]
quickTestBool str f = do
    samples :: [a] <- QC.sample' QC.arbitrary
    return [ msg ~: f s ~=? True
           | (i,s) <- zip [(1::Int)..] samples
           , let msg = "quickTest: " ++ str ++ "[" ++ show i ++ "]"
           ]


-- will use the Arbitrary instance to generate a random value of type a, and generate a sample of tests.
quickTest :: forall a . QC.Arbitrary a => (a -> Test) -> IO [Test]
quickTest f = do
    samples :: [a] <- QC.sample' QC.arbitrary
    return $ map f samples


runTest :: Test -> IO ()
runTest t = print =<< runTestTT t


runTestNTimes :: Int -> Test -> IO ()
runTestNTimes i t = print =<< runTestTT (test (replicate i t))
