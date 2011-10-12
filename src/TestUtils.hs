module TestUtils ( quickTest, runTest ) where


import Test.HUnit ( Test, (~:), (~=?), runTestTT )
import qualified Test.QuickCheck as QC ( Testable )
import qualified Test.QuickCheck.Test as QC ( isSuccess, quickCheckResult )


-- given a string and a QuickCheck property, constructs a HUnit test
quickTest :: QC.Testable prop => String -> prop -> IO Test
quickTest s p = do
    testResult <- QC.quickCheckResult p
    return $ "quickTest: " ++ s ~: True ~=? QC.isSuccess testResult


runTest :: IO Test -> IO ()
runTest t = print =<< runTestTT =<< t
