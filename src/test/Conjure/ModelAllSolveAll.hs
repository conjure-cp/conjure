{-# LANGUAGE RecordWildCards #-}

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
import qualified Data.Text as T ( Text, null, unpack )

-- containers
import qualified Data.Set as S ( fromList, toList, null, difference )


srOptions :: [T.Text]
srOptions =
    [ "-timelimit"      , "300000"
    , "-run-solver"     , "-minion"
    , "-solver-options" , "-cpulimit 300"
    , "-O0"
    ]


tests :: IO TestTree
tests = do
    let baseDir = "tests/exhaustive"
    dirs <- mapM (isTestDir baseDir) =<< getDirectoryContents baseDir
    let testCases = map testSingleDir (catMaybes dirs)
    return (testGroup "exhaustive" testCases)


data TestDirFiles = TestDirFiles
    { name           :: String          -- a name for the test case
    , tBaseDir       :: FilePath        -- dir
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
            expecteds <- do
                let dir = baseDir </> possiblyDir </> "expected"
                isDir <- doesDirectoryExist dir
                if isDir
                    then getDirectoryContents dir
                    else return []
            return TestDirFiles
                { name           = possiblyDir
                , tBaseDir       = baseDir </> possiblyDir
                , outputsDir     = baseDir </> possiblyDir </> "outputs"
                , expectedsDir   = baseDir </> possiblyDir </> "expected"
                , essenceFile    = baseDir </> possiblyDir </> possiblyDir ++ ".essence"
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
testSingleDir t@(TestDirFiles{..}) = testGroup name (conjuring : savileRows ++ checkingExpected ++ [extraFiles])
    where
        conjuring =
            testCase "Conjuring" $ do
                -- tl;dr: rm -rf outputsDir
                -- removeDirectoryRecursive gets upset if the dir doesn't exist.
                -- terrible solution: create the dir if it doesn't exists, rm -rf after that.
                createDirectoryIfMissing True outputsDir >> removeDirectoryRecursive outputsDir

                -- read in the essence, generate the eprimes
                essence <- ignoreLogs $ readModelFromFile essenceFile
                modelAll outputsDir essence

        savileRows =
            if null paramFiles
                then [ savileRowNoParam    t m   | m <- expectedModels ]
                else [ savileRowWithParams t m p | m <- expectedModels
                                                 , p <- paramFiles     ]

        checkingExpected =
            [ checkExpected t e | e <- expectedModels ] ++
            [ checkExpected t e | e <- expectedSols   ]

        extraFiles = checkExtraFiles t


savileRowNoParam :: TestDirFiles -> FilePath -> TestTree
savileRowNoParam TestDirFiles{..} modelPath =
    testCase (unwords ["Savile Row:", modelPath]) $ sh $ do
        let outBase = dropExtension modelPath
        _stdoutSR <- run "savilerow" $
            [ "-in-eprime"      , stringToText $ outputsDir </> outBase ++ ".eprime"
            , "-out-minion"     , stringToText $ outputsDir </> outBase ++ ".eprime-minion"
            , "-out-aux"        , stringToText $ outputsDir </> outBase ++ ".eprime-aux"
            , "-out-info"       , stringToText $ outputsDir </> outBase ++ ".eprime-info"
            , "-out-solution"   , stringToText $ outputsDir </> outBase ++ ".eprime-solution"
            , "-all-solutions"
            ] ++ srOptions
        stderrSR <- lastStderr
        if not (T.null stderrSR)
            then liftIO $ assertFailure $ T.unpack stderrSR
            else do
                nbEprimeSolutions <- length . filter ((outBase ++ ".eprime-solution.") `isPrefixOf`)
                                          <$> liftIO (getDirectoryContents outputsDir)
                forM_ (take nbEprimeSolutions allNats) $ \ i -> liftIO $ do
                    let eprimeSolutionPath = outBase ++ ".eprime-solution." ++ paddedNum i
                    eprimeModel    <- readModelFromFile (outputsDir </> modelPath)
                    eprimeSolution <- readModelFromFile (outputsDir </> eprimeSolutionPath)
                    s <- ignoreLogs $ translateSolution eprimeModel def eprimeSolution
                    let filename = outputsDir </> outBase ++ "-solution" ++ paddedNum i ++ ".solution"
                    writeFile filename (renderWide s)


savileRowWithParams :: TestDirFiles -> FilePath -> FilePath -> TestTree
savileRowWithParams TestDirFiles{..} modelPath paramPath =
    testCase (unwords ["Savile Row:", modelPath, paramPath]) $ sh $ do
        model       <- liftIO $ readModelFromFile (outputsDir </> modelPath)
        param       <- liftIO $ readModelFromFile (tBaseDir   </> paramPath)
        eprimeParam <- liftIO $ ignoreLogs $ refineParam model param
        let outBase = dropExtension modelPath ++ "-" ++ dropExtension paramPath
        liftIO $ writeFile (outputsDir </> outBase ++ ".eprime-param") (renderWide eprimeParam)
        _stdoutSR <- run "savilerow" $
            [ "-in-eprime"      , stringToText $ outputsDir </> modelPath
            , "-in-param"       , stringToText $ outputsDir </> outBase ++ ".eprime-param"
            , "-out-minion"     , stringToText $ outputsDir </> outBase ++ ".eprime-minion"
            , "-out-aux"        , stringToText $ outputsDir </> outBase ++ ".eprime-aux"
            , "-out-info"       , stringToText $ outputsDir </> outBase ++ ".eprime-info"
            , "-out-solution"   , stringToText $ outputsDir </> outBase ++ ".eprime-solution"
            , "-all-solutions"
            ] ++ srOptions
        stderrSR <- lastStderr
        if not (T.null stderrSR)
            then liftIO $ assertFailure $ T.unpack stderrSR
            else do
                nbEprimeSolutions <- length . filter ((outBase ++ ".eprime-solution.") `isPrefixOf`)
                                          <$> liftIO (getDirectoryContents outputsDir)
                forM_ (take nbEprimeSolutions allNats) $ \ i -> liftIO $ do
                    let eprimeSolutionPath = outBase ++ ".eprime-solution." ++ paddedNum i
                    eprimeModel    <- readModelFromFile (outputsDir </> modelPath)
                    eprimeSolution <- readModelFromFile (outputsDir </> eprimeSolutionPath)
                    case ignoreLogs (translateSolution eprimeModel param eprimeSolution) of
                        Left err -> assertFailure $ renderNormal err
                        Right s  -> do
                            let filename = outputsDir </> outBase ++ "-solution" ++ paddedNum i ++ ".solution"
                            writeFile filename (renderWide s)


checkExpected :: TestDirFiles -> FilePath -> TestTree
checkExpected TestDirFiles{..} item =
    testCase (unwords ["Checking expected:", item]) $ do
        let expectedPath  = expectedsDir </> item
        let generatedPath = outputsDir   </> item
        isFile <- doesFileExist generatedPath
        if isFile
            then do
                e <- readModelFromFile expectedPath
                g <- readModelFromFile generatedPath
                case modelDiff e g of
                    Nothing -> return ()
                    Just msg -> assertFailure $ renderWide $ "files differ:" <+> msg
            else assertFailure $ "file doesn't exist: " ++ generatedPath


checkExtraFiles :: TestDirFiles -> TestTree
checkExtraFiles TestDirFiles{..} =
    testCase "Checking extra files" $ do
        let modelOrSolution f = or [ suffix `isSuffixOf` f
                                   | suffix <- [".eprime", ".solution"]
                                   ]

        dirShouldExist expectedsDir
        dirShouldExist outputsDir
        expecteds <- S.fromList . filter modelOrSolution <$> getDirectoryContents expectedsDir
        outputs   <- S.fromList . filter modelOrSolution <$> getDirectoryContents outputsDir
        let extras = S.difference outputs expecteds
        unless (S.null extras) $
            assertFailure $ show $ "extra files:" <+> prettyList id ", " (S.toList extras)


dirShouldExist :: FilePath -> IO ()
dirShouldExist d = do
    b <- doesDirectoryExist d
    unless b $
        assertFailure $ "dir does not exist: " ++ d


modelAll :: FilePath -> Model -> IO ()
modelAll dir = ignoreLogs . outputModels def { strategyQ = PickFirst
                                             , strategyA = PickAll
                                             , outputDirectory = dir
                                             }


sh :: Sh a -> IO a
sh = shelly . print_stdout False . print_stderr False

