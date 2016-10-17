{-# LANGUAGE RecordWildCards #-}

module Conjure.Custom ( tests ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Pretty ( pretty, (<++>), renderNormal )

-- tasty
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCaseSteps, assertFailure )

-- text
import Data.Text as T ( null )
import Data.Text.IO as T ( writeFile )

-- shelly
import Shelly ( cd, bash, errExit, lastStderr )

-- system-filepath
import Filesystem.Path.CurrentOS as Path ( fromText )


tests :: IO TestTree
tests = do
    let baseDir = "tests/custom"
    dirs <- mapM (isTestDir baseDir) =<< getAllDirs baseDir
    let testCases = map testSingleDir (catMaybes dirs)
    return (testGroup "custom" testCases)


data TestDirFiles = TestDirFiles
    { name           :: String          -- a name for the test case
    , tBaseDir       :: FilePath        -- dir
    }
    deriving Show


-- returns True if the argument points to a directory that is not hidden
isTestDir :: FilePath -> FilePath -> IO (Maybe TestDirFiles)
isTestDir baseDir dir = do
    runSh <- filter ("run.sh" ==) <$> getDirectoryContents dir
    case runSh of
        [_] -> return $ Just TestDirFiles
            { name           = drop (length baseDir + 1) dir
            , tBaseDir       = dir
            }
        _ -> return Nothing


-- the first FilePath is the base directory for the parse_print tests
-- we know at this point that the second FilePath points to a directory D,
-- which contains + an Essence file D/D.essence
testSingleDir :: TestDirFiles -> TestTree
testSingleDir TestDirFiles{..} = testCaseSteps name $ \ step -> do
    step "Running"
    (stdout, stderr) <- sh $ errExit False $ do
        -- stdout <- run (tBaseDir </> "run.sh") []
        cd (Path.fromText $ stringToText $ tBaseDir)
        stdout <- bash "./run.sh" []
        stderr <- lastStderr
        return (stdout, stderr)
    unless (T.null stdout) $ T.writeFile (tBaseDir </> "stdout") stdout
    unless (T.null stderr) $ T.writeFile (tBaseDir </> "stderr") stderr

    let
        readIfExists :: FilePath -> IO String
        readIfExists f = fromMaybe "" <$> readFileIfExists f

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
