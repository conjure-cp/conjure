module Conjure.TypeCheckAll ( tests ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.NameGen ( runNameGen )
import Conjure.Language.Pretty ( renderNormal )
import Conjure.UI.IO
import Conjure.UI.TypeCheck
import Conjure.UserError ( runUserErrorT )

-- tasty
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, assertFailure )


tests :: IO TestTree
tests = do
    let baseDir = "tests/exhaustive"
    files <- getAllFilesWithSuffix ".essence" baseDir
    let testCases = map testSingle files
    return (testGroup "type-checking" testCases)

testSingle :: FilePath -> TestTree
testSingle fp = testCase fp $ do
    model <- readModelFromFile fp
    result <- runUserErrorT $ ignoreLogs $ runNameGen $ typeCheckModel_StandAlone model
    case result of
        Left errs -> assertFailure $ renderNormal $ vcat errs
        Right _ -> return ()
