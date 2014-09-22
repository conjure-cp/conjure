module Conjure.ModelAllSolveAll ( tests ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Pretty
import Conjure.UI.IO
import Conjure.UI.Model
import Conjure.UI.RefineParam
import Conjure.UI.TranslateSolution

-- tasty
import Test.Tasty
import Test.Tasty.HUnit

-- shelly
import Shelly ( Sh, shelly, run, lastStderr, print_stdout )

-- text
import qualified Data.Text as T ( null, unpack )


tests :: IO TestTree
tests = do
    let baseDir = "src/test/exhaustive"
    dirs <- filterM (isTestDir baseDir) =<< getDirectoryContents baseDir
    let testCases = map (testSingleDir baseDir) dirs
    return (testGroup "exhaustive" testCases)


-- returns True if the argument points to a directory that is not hidden
isTestDir :: FilePath -> FilePath -> IO Bool
isTestDir baseDir possiblyDir =
    (&&) <$> (doesDirectoryExist (baseDir </> possiblyDir))
         <*> (doesFileExist (baseDir </> possiblyDir </> possiblyDir ++ ".essence"))


-- the first FilePath is the base directory for the exhaustive tests
-- we know at this point that the second FilePath points to a directory D,
-- which contains + an Essence file D/D.essence
--                + D/*.param files if required
--                + D/expected for the expected output files
testSingleDir :: FilePath -> TestName -> TestTree
testSingleDir baseDir basename = testCase basename $ do
    let wd = baseDir </> basename
    let outputsDir = wd </> "outputs"
    -- removeDirectoryRecursive outputsDir
    essence <- readModelFromFile (wd </> basename ++ ".essence")
    modelAll outputsDir essence
    models <- filter (".eprime" `isSuffixOf`) <$> getDirectoryContents outputsDir
    params <- filter (".param"  `isSuffixOf`) <$> getDirectoryContents wd
    if null params
        then shelly $ print_stdout False
                    $ testSingleDirNoParam outputsDir models
        else shelly $ print_stdout False
                    $ testSingleDirWithParams outputsDir models wd params


testSingleDirNoParam :: FilePath -> [FilePath] -> Sh ()
testSingleDirNoParam outputsDir models =
    forM_ models $ \ model -> do
        let outBase = dropExtension model
        _stdoutSR <- run "savilerow"
            [ "-in-eprime"      , stringToText $ outputsDir </> outBase ++ ".eprime"
            , "-out-minion"     , stringToText $ outputsDir </> outBase ++ ".eprime-minion"
            , "-out-aux"        , stringToText $ outputsDir </> outBase ++ ".eprime-aux"
            , "-out-info"       , stringToText $ outputsDir </> outBase ++ ".eprime-info"
            , "-out-solution"   , stringToText $ outputsDir </> outBase ++ ".eprime-solution"
            , "-timelimit"      , "300000"
            , "-run-solver"     , "-minion"
            , "-solver-options" , "-cpulimit 300"
            , "-O0"
            , "-all-solutions"
            ]
        stderrSR <- lastStderr
        unless (T.null stderrSR)
               (liftIO $ assertFailure $ T.unpack stderrSR)
        eprimeSolutions <- filter ((outBase ++ ".eprime-solution.") `isPrefixOf`)
                                  <$> liftIO (getDirectoryContents outputsDir)
        forM_ (zip allNats eprimeSolutions) $ \ (i, eprimeSolutionPath) -> liftIO $ do
            eprimeModel    <- readModelFromFile (outputsDir </> model)
            eprimeSolution <- readModelFromFile (outputsDir </> eprimeSolutionPath)
            case translateSolution eprimeModel def eprimeSolution of
                Left err -> assertFailure $ renderNormal err
                Right s -> do
                    let filename = outputsDir </> outBase ++ "-solution" ++ show i ++ ".solution"
                    writeFile filename (renderWide s)


testSingleDirWithParams :: FilePath -> [FilePath] -> FilePath -> [FilePath] -> Sh ()
testSingleDirWithParams outputsDir models paramsDir params =
    forM_ params $ \ paramPath -> forM_ models $ \ modelPath -> do
        model <- liftIO $ readModelFromFile (outputsDir </> modelPath)
        param <- liftIO $ readModelFromFile (paramsDir  </> paramPath)
        case refineParam model param of
            Left err -> liftIO $ assertFailure $ renderNormal err
            Right eprimeParam -> do
                let outBase = dropExtension modelPath ++ "-" ++ dropExtension paramPath
                -- liftIO $ writeFile (outputsDir </> outBase ++ ".eprime-param") (renderWide eprimeParam)
                _stdoutSR <- run "savilerow"
                    [ "-in-eprime"      , stringToText $ outputsDir </> modelPath
                    , "-in-param"       , stringToText $ outputsDir </> outBase ++ ".eprime-param"
                    , "-out-minion"     , stringToText $ outputsDir </> outBase ++ ".eprime-minion"
                    , "-out-aux"        , stringToText $ outputsDir </> outBase ++ ".eprime-aux"
                    , "-out-info"       , stringToText $ outputsDir </> outBase ++ ".eprime-info"
                    , "-out-solution"   , stringToText $ outputsDir </> outBase ++ ".eprime-solution"
                    , "-timelimit"      , "300000"
                    , "-run-solver"     , "-minion"
                    , "-solver-options" , "-cpulimit 300"
                    , "-O0"
                    , "-all-solutions"
                    ]
                stderrSR <- lastStderr
                unless (T.null stderrSR)
                       (liftIO $ assertFailure $ T.unpack stderrSR)
                eprimeSolutions <- filter ((outBase ++ ".eprime-solution.") `isPrefixOf`)
                                          <$> liftIO (getDirectoryContents outputsDir)
                forM_ (zip allNats eprimeSolutions) $ \ (i, eprimeSolutionPath) -> liftIO $ do
                    eprimeModel    <- readModelFromFile (outputsDir </> modelPath)
                    eprimeSolution <- readModelFromFile (outputsDir </> eprimeSolutionPath)
                    case translateSolution eprimeModel param eprimeSolution of
                        Left err -> assertFailure $ renderNormal err
                        Right s  -> do
                            let filename = outputsDir </> outBase ++ "-solution" ++ show i ++ ".solution"
                            writeFile filename (renderWide s)


modelAll :: FilePath -> Model -> IO ()
modelAll dir essence = outputAllModels dir 1 (initialise essence)


dropExtension :: FilePath -> FilePath
dropExtension = intercalate "." . init . splitOn "."

