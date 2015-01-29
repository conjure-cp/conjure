module Conjure.TypeCheckAll ( tests ) where

-- conjure
import Conjure.Prelude
import Conjure.UI.IO
import Conjure.UI.TypeCheck

-- tasty
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCase )


tests :: IO TestTree
tests = do
    let baseDir = "tests"
    files <- allFilesWithSuffix ".essence" baseDir
    let testCases = map testSingle files
    return (testGroup "type-checking" testCases)

testSingle :: FilePath -> TestTree
testSingle fp = testCase fp $ do
    model <- readModelFromFile fp
    ignoreLogs $ typeCheckModel model
