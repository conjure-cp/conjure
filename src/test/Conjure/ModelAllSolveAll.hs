module Conjure.ModelAllSolveAll ( tests ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Pretty
import Conjure.Language.ModelDiff
import Conjure.UI.IO
import Conjure.UI.Model
import Conjure.UI.RefineParam
import Conjure.UI.TranslateSolution

-- tasty
import Test.Tasty
import Test.Tasty.HUnit

-- shelly
import Shelly ( Sh, shelly, run, lastStderr, print_stdout, print_stderr )

-- text
import qualified Data.Text as T ( null, unpack )


tests :: IO TestTree
tests = do
    let baseDir = "src/test/exhaustive"
    dirs <- mapM (isTestDir baseDir) =<< getDirectoryContents baseDir
    let testCases = map testSingleDir (catMaybes dirs)
    return (testGroup "exhaustive" testCases)


data TestDirFiles = TestDirFiles
    { name           :: String          -- a name for the test case
    , tBaseDir        :: FilePath        -- dir
    , outputsDir     :: FilePath        -- dir
    , expectedsDir   :: FilePath        -- dir
    , essenceFile    :: FilePath        -- dir + filename
    , paramFiles     :: [FilePath]      -- filename
    , expectedModels :: [FilePath]      -- filename
    , expectedSols   :: [FilePath]      -- filename
    }
    deriving Show


-- returns True if the argument points to a directory that is not hidden
isTestDir :: FilePath -> FilePath -> IO (Maybe TestDirFiles)
isTestDir baseDir possiblyDir = do
    b <- (&&) <$> doesDirectoryExist (baseDir </> possiblyDir)
              <*> doesFileExist (baseDir </> possiblyDir </> possiblyDir ++ ".essence")
    if not b
        then return Nothing
        else Just <$> do
            params    <- filter (".param"  `isSuffixOf`) <$> getDirectoryContents (baseDir </> possiblyDir)
            expecteds <- getDirectoryContents (baseDir </> possiblyDir </> "expected")
            return TestDirFiles 
                { name           = possiblyDir
                , essenceFile    = baseDir </> possiblyDir </> possiblyDir ++ ".essence"
                , tBaseDir       = baseDir </> possiblyDir
                , outputsDir     = baseDir </> possiblyDir </> "outputs"
                , expectedsDir   = baseDir </> possiblyDir </> "expected"
                , paramFiles     = params
                , expectedModels = filter (".eprime"   `isSuffixOf`) expecteds
                , expectedSols   = filter (".solution" `isSuffixOf`) expecteds
                }


-- the first FilePath is the base directory for the exhaustive tests
-- we know at this point that the second FilePath points to a directory D,
-- which contains + an Essence file D/D.essence
--                + D/*.param files if required
--                + D/expected for the expected output files
testSingleDir :: TestDirFiles -> TestTree
testSingleDir t = testGroup (name t) (conjuring : savileRows ++ checkingExpected)
    where
        conjuring =
            testCase "Conjuring" $ do
                -- tl;dr: rm -rf outputsDir
                -- removeDirectoryRecursive gets upset if the dir doesn't exist.
                -- terrible solution: create the dir if it doesn't exists, rm -rf after that.
                createDirectoryIfMissing True (outputsDir t) >> removeDirectoryRecursive (outputsDir t)

                -- read in the essence, generate the eprimes
                essence <- readModelFromFile (essenceFile t)
                modelAll (outputsDir t) essence

        savileRows =
            if null (paramFiles t)
                then [ testSingleDirNoParam    t m   | m <- expectedModels t ]
                else [ testSingleDirWithParams t m p | m <- expectedModels t
                                                     , p <- paramFiles     t ]

        checkingExpected =
            [ checkExpected t e | e <- expectedModels t ] ++
            [ checkExpected t e | e <- expectedSols   t ]


testSingleDirNoParam :: TestDirFiles -> FilePath -> TestTree
testSingleDirNoParam t modelPath =
    testCase (unwords ["Savile Row:", modelPath]) $ sh $ do
        let outBase = dropExtension modelPath
        _stdoutSR <- run "savilerow"
            [ "-in-eprime"      , stringToText $ outputsDir t </> outBase ++ ".eprime"
            , "-out-minion"     , stringToText $ outputsDir t </> outBase ++ ".eprime-minion"
            , "-out-aux"        , stringToText $ outputsDir t </> outBase ++ ".eprime-aux"
            , "-out-info"       , stringToText $ outputsDir t </> outBase ++ ".eprime-info"
            , "-out-solution"   , stringToText $ outputsDir t </> outBase ++ ".eprime-solution"
            , "-timelimit"      , "300000"
            , "-run-solver"     , "-minion"
            , "-solver-options" , "-cpulimit 300"
            , "-O0"
            , "-all-solutions"
            ]
        stderrSR <- lastStderr
        if not (T.null stderrSR)
            then liftIO $ assertFailure $ T.unpack stderrSR
            else do
                nbEprimeSolutions <- length . filter ((outBase ++ ".eprime-solution.") `isPrefixOf`)
                                          <$> liftIO (getDirectoryContents (outputsDir t))
                forM_ (take nbEprimeSolutions allNats) $ \ i -> liftIO $ do
                    let eprimeSolutionPath = outBase ++ ".eprime-solution." ++ paddedNum i
                    eprimeModel    <- readModelFromFile (outputsDir t </> modelPath)
                    eprimeSolution <- readModelFromFile (outputsDir t </> eprimeSolutionPath)
                    case translateSolution eprimeModel def eprimeSolution of
                        Left err -> assertFailure $ renderNormal err
                        Right s -> do
                            let filename = outputsDir t </> outBase ++ "-solution" ++ paddedNum i ++ ".solution"
                            writeFile filename (renderWide s)


testSingleDirWithParams :: TestDirFiles -> FilePath -> FilePath -> TestTree
testSingleDirWithParams t modelPath paramPath =
    testCase (unwords ["Savile Row:", modelPath, paramPath]) $ sh $ do
        model <- liftIO $ readModelFromFile (outputsDir t </> modelPath)
        param <- liftIO $ readModelFromFile (tBaseDir   t </> paramPath)
        case refineParam model param of
            Left err -> liftIO $ assertFailure $ renderNormal err
            Right eprimeParam -> do
                let outBase = dropExtension modelPath ++ "-" ++ dropExtension paramPath
                liftIO $ writeFile (outputsDir t </> outBase ++ ".eprime-param") (renderWide eprimeParam)
                _stdoutSR <- run "savilerow"
                    [ "-in-eprime"      , stringToText $ outputsDir t </> modelPath
                    , "-in-param"       , stringToText $ outputsDir t </> outBase ++ ".eprime-param"
                    , "-out-minion"     , stringToText $ outputsDir t </> outBase ++ ".eprime-minion"
                    , "-out-aux"        , stringToText $ outputsDir t </> outBase ++ ".eprime-aux"
                    , "-out-info"       , stringToText $ outputsDir t </> outBase ++ ".eprime-info"
                    , "-out-solution"   , stringToText $ outputsDir t </> outBase ++ ".eprime-solution"
                    , "-timelimit"      , "300000"
                    , "-run-solver"     , "-minion"
                    , "-solver-options" , "-cpulimit 300"
                    , "-O0"
                    , "-all-solutions"
                    ]
                stderrSR <- lastStderr
                if not (T.null stderrSR)
                    then liftIO $ assertFailure $ T.unpack stderrSR
                    else do
                        nbEprimeSolutions <- length . filter ((outBase ++ ".eprime-solution.") `isPrefixOf`)
                                                  <$> liftIO (getDirectoryContents (outputsDir t))
                        forM_ (take nbEprimeSolutions allNats) $ \ i -> liftIO $ do
                            let eprimeSolutionPath = outBase ++ ".eprime-solution." ++ paddedNum i
                            eprimeModel    <- readModelFromFile (outputsDir t </> modelPath)
                            eprimeSolution <- readModelFromFile (outputsDir t </> eprimeSolutionPath)
                            case translateSolution eprimeModel param eprimeSolution of
                                Left err -> assertFailure $ renderNormal err
                                Right s  -> do
                                    let filename = outputsDir t </> outBase ++ "-solution" ++ paddedNum i ++ ".solution"
                                    writeFile filename (renderWide s)


checkExpected :: TestDirFiles -> FilePath -> TestTree
checkExpected t item =
    testCase (unwords ["Checking expected:", item]) $ do
        let expectedPath  = expectedsDir t </> item
        let generatedPath = outputsDir   t </> item
        isFile <- doesFileExist generatedPath
        if isFile
            then do
                e <- readModelFromFile expectedPath
                g <- readModelFromFile generatedPath
                case modelDiff e g of
                    Nothing -> return ()
                    Just msg -> assertFailure $ renderWide $ "files differ:" <+> msg
            else assertFailure $ "file doesn't exist: " ++ generatedPath


modelAll :: FilePath -> Model -> IO ()
modelAll dir essence = outputModels allFixedQs dir 1 essence


dropExtension :: FilePath -> FilePath
dropExtension = intercalate "." . init . splitOn "."


sh :: Sh a -> IO a
sh = shelly . print_stdout False . print_stderr False

