module Conjure.TypeCheckAll ( tests ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.NameGen ( runNameGen )
import Conjure.Language.Pretty ( renderNormal )
import Conjure.Language.TypeOf ( TypeCheckerMode(..) )
import Conjure.UI.IO
import Conjure.UI.TypeCheck
import Conjure.UserError ( runUserErrorT )

-- tasty
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, assertFailure )


tests ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    IO TestTree
tests = do
    let baseDir = "tests/exhaustive"
    files <- getAllFilesWithSuffix ".essence" baseDir
    let testCases = map testSingle files
    return (testGroup "type-checking" testCases)

testSingle ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    FilePath -> TestTree
testSingle fp = testCase (map (\ ch -> if ch == '/' then '.' else ch) fp) $ do
    model <- readModelFromFile fp
    result <- runUserErrorT $ ignoreLogs $ runNameGen () $ typeCheckModel_StandAlone model
    case result of
        Left errs -> assertFailure $ renderNormal $ vcat errs
        Right _ -> return ()
