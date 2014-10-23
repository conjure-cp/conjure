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
    dirs <- filterM (isTestDir baseDir) =<< getDirectoryContents baseDir
    testCases <- mapM (testSingleDir baseDir) dirs
    return (testGroup "exhaustive" testCases)


-- returns True if the argument points to a directory that is not hidden
isTestDir :: FilePath -> FilePath -> IO Bool
isTestDir baseDir possiblyDir =
    (&&) <$> doesDirectoryExist (baseDir </> possiblyDir)
         <*> doesFileExist (baseDir </> possiblyDir </> possiblyDir ++ ".essence")


-- the first FilePath is the base directory for the exhaustive tests
-- we know at this point that the second FilePath points to a directory D,
-- which contains + an Essence file D/D.essence
--                + D/*.param files if required
--                + D/expected for the expected output files
testSingleDir :: FilePath -> TestName -> IO TestTree
testSingleDir baseDir basename = do
    conjuring <- do

            -- the working directory and the outputs directory
            let wd = baseDir </> basename
            let outputsDir = wd </> "outputs"

            -- tl;dr: rm -rf outputsDir
            -- removeDirectoryRecursive gets upset if the dir doesn't exist.
            -- terrible solution: create the dir if it doesn't exists, rm -rf after that.
            createDirectoryIfMissing True outputsDir >> removeDirectoryRecursive outputsDir

            -- read in the essence, generate the eprimes
            essence <- readModelFromFile (wd </> basename ++ ".essence")
            conjuringResult <- modelAll outputsDir essence

            -- eprime's generates, and stored under outputs/*.eprime
            models <- filter (".eprime" `isSuffixOf`) <$> getDirectoryContents outputsDir
            -- essence params will be at *.param, if they exist
            params <- filter (".param"  `isSuffixOf`) <$> getDirectoryContents wd

            -- SavileRow+Minion for every model and every param
            srResults <-
                shelly $ print_stdout False
                       $ print_stderr False
                       $ if null params
                            then testSingleDirNoParam outputsDir models
                            else testSingleDirWithParams outputsDir models wd params

            return $
                ( testCase "Conjuring" $
                    either (assertFailure . renderNormal) 
                           (const (return ()))
                           conjuringResult
                ) :
                [ testCase name $
                    maybe  (return ())
                           (assertFailure . renderNormal)
                           res
                | (name, res) <- srResults
                ]

    -- check if the output models+solutions are the same as
    --          the expected ones
    checkingExpected <- checkExpected (baseDir </> basename </> "expected")
                                      (baseDir </> basename </> "outputs" )

    -- "checkingExpected" tests do depend on "conjuring" being run first
    -- hence we cannot exploit parallelism with this kind of a test description.
    -- will use -j1 for now.
    return $ testGroup basename (conjuring ++ checkingExpected)


testSingleDirNoParam :: FilePath -> [FilePath] -> Sh [(String, Maybe String)]
testSingleDirNoParam outputsDir models =
    forM models $ \ model -> do
        let testCaseName = unwords ["Savile Row:", model]
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
        if not (T.null stderrSR)
            then return ( testCaseName
                        , Just (T.unpack stderrSR)
                        )
            else do
                eprimeSolutions <- filter ((outBase ++ ".eprime-solution.") `isPrefixOf`)
                                          <$> liftIO (getDirectoryContents outputsDir)
                forM_ (zip allNats eprimeSolutions) $ \ (i, eprimeSolutionPath) -> liftIO $ do
                    eprimeModel    <- readModelFromFile (outputsDir </> model)
                    eprimeSolution <- readModelFromFile (outputsDir </> eprimeSolutionPath)
                    case translateSolution eprimeModel def eprimeSolution of
                        Left err -> assertFailure $ renderNormal (err :: Doc)
                        Right s -> do
                            let filename = outputsDir </> outBase ++ "-solution" ++ show i ++ ".solution"
                            writeFile filename (renderWide s)
                return (testCaseName, Nothing)


testSingleDirWithParams :: FilePath -> [FilePath] -> FilePath -> [FilePath] -> Sh [(String, Maybe String)]
testSingleDirWithParams outputsDir models paramsDir params =
    liftM concat $ forM params $ \ paramPath -> forM models $ \ modelPath -> do
        let testCaseName = unwords ["Savile Row:", modelPath, "~~", paramPath]
        model <- liftIO $ readModelFromFile (outputsDir </> modelPath)
        param <- liftIO $ readModelFromFile (paramsDir  </> paramPath)
        case refineParam model param of
            Left err -> return (testCaseName, Just (renderNormal err))
            Right eprimeParam -> do
                let outBase = dropExtension modelPath ++ "-" ++ dropExtension paramPath
                liftIO $ writeFile (outputsDir </> outBase ++ ".eprime-param") (renderWide eprimeParam)
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
                if not (T.null stderrSR)
                    then return ( testCaseName
                                , Just (T.unpack stderrSR)
                                )
                    else do
                        eprimeSolutions <- filter ((outBase ++ ".eprime-solution.") `isPrefixOf`)
                                                  <$> liftIO (getDirectoryContents outputsDir)
                        forM_ (zip allNats eprimeSolutions) $ \ (i, eprimeSolutionPath) -> liftIO $ do
                            eprimeModel    <- readModelFromFile (outputsDir </> modelPath)
                            eprimeSolution <- readModelFromFile (outputsDir </> eprimeSolutionPath)
                            case translateSolution eprimeModel param eprimeSolution of
                                Left err -> assertFailure $ renderNormal (err :: Doc)
                                Right s  -> do
                                    let filename = outputsDir </> outBase ++ "-solution" ++ show i ++ ".solution"
                                    writeFile filename (renderWide s)
                        return (testCaseName, Nothing)


checkExpected :: FilePath -> FilePath -> IO [TestTree]
checkExpected expected generated = do
    expectedDirExists <- doesDirectoryExist expected
    if not expectedDirExists
        then return [testCase "Expected" $ assertFailure ("Directory does not exist: " ++ show expected)]
        else do
            expecteds <- filter (not . ("." `isPrefixOf`)) <$> getDirectoryContents expected
            forM expecteds $ \ item -> do
                let expectedPath  = expected  </> item
                let generatedPath = generated </> item
                isFile <- doesFileExist generatedPath
                if isFile
                    then do
                        e <- readModelFromFile expectedPath
                        g <- readModelFromFile generatedPath
                        return $ testCase item $
                            case modelDiff e g of
                                Nothing -> return ()
                                Just msg -> assertFailure $ renderWide $ "files differ:" <+> msg
                    else do
                        cont <- getDirectoryContents generated
                        return $ testCase ("Diff, " ++ item) (assertFailure $ "file doesn't exist: " ++ generatedPath ++ show cont)


modelAll :: FilePath -> Model -> IO (Either Doc ())
modelAll dir essence = runExceptT (outputModels allFixedQs dir 1 essence)


dropExtension :: FilePath -> FilePath
dropExtension = intercalate "." . init . splitOn "."

