{-# LANGUAGE RecordWildCards #-}

module Conjure.ParsePrint ( tests ) where

-- conjure
import Conjure.Prelude
import Conjure.UserError
import Conjure.Language.Definition ( Model )
import Conjure.Language.Pretty ( pretty, (<++>), renderNormal )
import Conjure.Language.NameGen ( runNameGen )
import Conjure.UI ( OutputFormat(..) )
import Conjure.UI.IO ( readModelFromFile, writeModel )
import Conjure.UI.TypeCheck ( typeCheckModel_StandAlone )

-- base
import System.Info ( os )

-- tasty
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCaseSteps, assertFailure )


tests :: IO TestTree
tests = do
    let baseDir = "tests/parse_print"
    dirs <- mapM (isTestDir baseDir) =<< getAllDirs baseDir
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
isTestDir baseDir dir = do
    essenceFiles <- filter (".essence" `isSuffixOf`) <$> getDirectoryContents dir
    case essenceFiles of
        [f] -> return $ Just TestDirFiles
            { name           = drop (length baseDir + 1) dir
            , tBaseDir       = dir
            , essenceFile    = dir </> f
            }
        _ -> return Nothing


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
            Left err    -> return (userErr err)
            Right model ->
                case tyCheck model of
                    Left err -> return (Left err)
                    Right () -> return (Right model)
    case result of
        Left ""     -> do
            removeFileIfExists (tBaseDir </> "stderr")
            removeFileIfExists (tBaseDir </> "stdout")
        Left err    -> do
            writeFile (tBaseDir </> "stderr") (renderNormal err)
            removeFileIfExists (tBaseDir </> "stdout")
        Right model -> do
            writeModel 120 Plain (Just (tBaseDir </> "stdout")) model
            removeFileIfExists (tBaseDir </> "stderr")

    let

        fixWindowsPaths :: String -> String
        fixWindowsPaths
            | os `elem` ["mingw32"] = fixBackslashes
            | otherwise             = id

        fixBackslashes :: String -> String
        fixBackslashes ('/'  : '\\' : xs) = "/\\" ++ fixBackslashes xs
        fixBackslashes ('\\' : '/'  : xs) = "\\/" ++ fixBackslashes xs
        fixBackslashes ('\\'        : xs) = '/'    : fixBackslashes xs
        fixBackslashes [] = []
        fixBackslashes (x:xs) = x : fixBackslashes xs

        readIfExists :: FilePath -> IO String
        readIfExists f = fromMaybe "" <$> readFileIfExists f

    step "Checking stdout"
    stdoutG <- fixWindowsPaths <$> readIfExists (tBaseDir </> "stdout")
    stdoutE <- readIfExists (tBaseDir </> "stdout.expected")
    unless (stdoutE == stdoutG) $
        assertFailure $ renderNormal $ vcat [ "unexpected stdout:" <++> pretty stdoutG
                                            , "was expecting:    " <++> pretty stdoutE ]
    step "Checking stderr"
    stderrG <- fixWindowsPaths <$> readIfExists (tBaseDir </> "stderr")
    stderrE <- readIfExists (tBaseDir </> "stderr.expected")
    unless (stderrE == stderrG) $
        assertFailure $ renderNormal $ vcat [ "unexpected stderr:" <++> pretty stderrG
                                            , "was expecting:    " <++> pretty stderrE ]
