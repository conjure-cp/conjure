{-# LANGUAGE RecordWildCards #-}

module Conjure.ParsePrint ( tests ) where

-- conjure
import Conjure.Prelude
import Conjure.UserError ( runUserErrorT )
import Conjure.Language.Definition ( Model )
import Conjure.Language.Pretty ( pretty, (<++>), renderNormal )
import Conjure.Language.NameGen ( runNameGen )
import Conjure.UI.IO ( readModelFromFile, writeModel, EssenceFileMode(..) )
import Conjure.UI.TypeCheck ( typeCheckModel_StandAlone )

-- base
import System.IO ( readFile )

-- tasty
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCaseSteps, assertFailure )


tests :: IO TestTree
tests = do
    let baseDir = "tests/parse_print"
    dirs <- mapM (isTestDir baseDir) =<< getDirectoryContents baseDir
    let testCases = map testSingleDir (catMaybes dirs)
    return (testGroup "parse_print" testCases)


data TestDirFiles = TestDirFiles
    { name           :: String          -- a name for the test case
    , tBaseDir       :: FilePath        -- dir
    , essenceFile    :: FilePath        -- dir + filename
    }
    deriving Show


-- returns True if the argument points to a directory that is not hidden
isTestDir :: FilePath -> FilePath -> IO (Maybe TestDirFiles)
isTestDir baseDir possiblyDir = do
    b <- (&&) <$> doesDirectoryExist (baseDir </> possiblyDir)
              <*> doesFileExist (baseDir </> possiblyDir </> possiblyDir ++ ".essence")
    if not b
        then return Nothing
        else return $ Just TestDirFiles
                { name           = possiblyDir
                , tBaseDir       = baseDir </> possiblyDir
                , essenceFile    = baseDir </> possiblyDir </> possiblyDir ++ ".essence"
                }


-- the first FilePath is the base directory for the parse_print tests
-- we know at this point that the second FilePath points to a directory D,
-- which contains + an Essence file D/D.essence
testSingleDir :: TestDirFiles -> TestTree
testSingleDir TestDirFiles{..} = testCaseSteps name $ \ step -> do
    step "Conjuring"
    model_ <- runUserErrorT (readModelFromFile essenceFile)
    let
        tyCheck :: Model -> Either Doc ()
        tyCheck m = runNameGen $ ignoreLogs $ void $ typeCheckModel_StandAlone m
    result <-
        case model_ of
            Left err    -> return (Left err)
            Right model ->
                case tyCheck model of
                    Left err -> return (Left err)
                    Right () -> return (Right model)
    case result of
        Left ""     -> return ()
        Left err    -> writeFile (tBaseDir </> "stderr") (renderNormal err)
        Right model -> writeModel PlainEssence (Just (tBaseDir </> "stdout")) model

    let
        readIfExists :: FilePath -> IO String
        readIfExists f = do
            e <- doesFileExist f
            if e
                then readFile f
                else return ""

    step "Checking stdout"
    stdoutG <- readIfExists (tBaseDir </> "stdout")
    stdoutE <- readIfExists (tBaseDir </> "stdout.expected")
    unless (stdoutE == stdoutG) $
        assertFailure $ renderNormal $ vcat [ "unexpected stdout:" <++> pretty stdoutG
                                            , "was expecting:    " <++> pretty stdoutE ]
    step "Checking stderr"
    stderrG <- readIfExists (tBaseDir </> "stderr")
    stderrE <- readIfExists (tBaseDir </> "stderr.expected")
    unless (stderrE == stderrG) $
        assertFailure $ renderNormal $ vcat [ "unexpected stderr:" <++> pretty stderrG
                                            , "was expecting:    " <++> pretty stderrE ]
