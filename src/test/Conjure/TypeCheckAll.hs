module Conjure.TypeCheckAll ( tests ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.NameGen ( runNameGen )
import Conjure.UI.IO
import Conjure.UI.TypeCheck

-- tasty
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCase )


tests :: IO TestTree
tests = do
    let baseDir = "tests/exhaustive"
    files <- allFilesWithSuffix ".essence" baseDir
    let testCases = map testSingle files
    return (testGroup "type-checking" testCases)

testSingle :: FilePath -> TestTree
testSingle fp = testCase fp $ do
    model <- readModelFromFile fp
    void $ ignoreLogs $ runNameGen $ typeCheckModel_StandAlone model
