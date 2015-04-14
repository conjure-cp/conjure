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
import Conjure.UI.ValidateSolution

-- base
import System.Environment ( getEnvironment )

-- tasty
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, testCaseSteps, assertFailure )

-- shelly
import Shelly ( run, lastStderr )

-- text
import qualified Data.Text as T ( Text, null, pack, unpack )

-- containers
import qualified Data.Set as S ( fromList, toList, empty, null, difference )


srOptions :: String -> [T.Text]
srOptions srExtraOptions =
    [ "-run-solver"
    , "-minion"
    -- , "-timelimit"      , "1200000"
    -- , "-solver-options" , "-cpulimit 1200"
    , "-all-solutions"
    , "-preprocess"     , "None"
    ] ++ map T.pack (words srExtraOptions)


tests :: IO TestTree
tests = do
    srExtraOptions <- do
        env <- getEnvironment
        return $ fromMaybe "-O0" (lookup "SR_OPTIONS" env)
    putStrLn $ "Using Savile Row options: " ++ unwords (map T.unpack (srOptions srExtraOptions))
    let baseDir = "tests/exhaustive"
    dirs <- mapM (isTestDir baseDir) =<< getDirectoryContents baseDir
    testCases <- mapM (testSingleDir srExtraOptions) (catMaybes dirs)
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
testSingleDir :: String -> TestDirFiles -> IO TestTree
testSingleDir srExtraOptions t@(TestDirFiles{..}) = do
    return $ testGroup name $ concat
        [ [conjuring]
        , savileRows
        , validating
        , [checkExpectedAndExtraFiles t]
        , [equalNumberOfSolutions t]
        ]
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
                then [ savileRowNoParam    srExtraOptions t m   | m <- expectedModels ]
                else [ savileRowWithParams srExtraOptions t m p | m <- expectedModels
                                                                , p <- paramFiles     ]

        validating =
            if null paramFiles
                then [ validateSolutionNoParam    t   s | s <- expectedSols   ]
                else [ validateSolutionWithParams t p s | p <- paramFiles
                                                        , s <- expectedSols
                                                        , dropExtension p `isInfixOf` dropExtension s
                                                        ]


savileRowNoParam :: String -> TestDirFiles -> FilePath -> TestTree
savileRowNoParam srExtraOptions TestDirFiles{..} modelPath =
    testCase (unwords ["Savile Row:", modelPath]) $ sh $ do
        let outBase = dropExtension modelPath
        _stdoutSR <- run "savilerow" $
            [ "-in-eprime"      , stringToText $ outputsDir </> outBase ++ ".eprime"
            , "-out-minion"     , stringToText $ outputsDir </> outBase ++ ".eprime-minion"
            , "-out-aux"        , stringToText $ outputsDir </> outBase ++ ".eprime-aux"
            , "-out-info"       , stringToText $ outputsDir </> outBase ++ ".eprime-info"
            , "-out-solution"   , stringToText $ outputsDir </> outBase ++ ".eprime-solution"
            ] ++ srOptions srExtraOptions
        stderrSR <- lastStderr
        if not (T.null stderrSR)
            then liftIO $ assertFailure $ T.unpack stderrSR
            else do
                eprimeModel       <- liftIO $ readModelFromFile (outputsDir </> modelPath)
                nbEprimeSolutions <- length . filter ((outBase ++ ".eprime-solution.") `isPrefixOf`)
                                          <$> liftIO (getDirectoryContents outputsDir)
                forM_ (take nbEprimeSolutions allNats) $ \ i -> liftIO $ do
                    let eprimeSolutionPath = outBase ++ ".eprime-solution." ++ paddedNum i
                    eprimeSolution <- readModelFromFile (outputsDir </> eprimeSolutionPath)
                    s <- ignoreLogs $ runNameGen $ translateSolution eprimeModel def eprimeSolution
                    let filename = outputsDir </> outBase ++ "-solution" ++ paddedNum i ++ ".solution"
                    writeFile filename (renderNormal s)


savileRowWithParams :: String -> TestDirFiles -> FilePath -> FilePath -> TestTree
savileRowWithParams srExtraOptions TestDirFiles{..} modelPath paramPath =
    testCase (unwords ["Savile Row:", modelPath, paramPath]) $ sh $ do
        model       <- liftIO $ readModelFromFile (outputsDir </> modelPath)
        param       <- liftIO $ readModelFromFile (tBaseDir   </> paramPath)
        eprimeParam <- liftIO $ ignoreLogs $ runNameGen $ refineParam model param
        let outBase = dropExtension modelPath ++ "-" ++ dropExtension paramPath
        liftIO $ writeFile (outputsDir </> outBase ++ ".eprime-param") (renderNormal eprimeParam)
        _stdoutSR <- run "savilerow" $
            [ "-in-eprime"      , stringToText $ outputsDir </> modelPath
            , "-in-param"       , stringToText $ outputsDir </> outBase ++ ".eprime-param"
            , "-out-minion"     , stringToText $ outputsDir </> outBase ++ ".eprime-minion"
            , "-out-aux"        , stringToText $ outputsDir </> outBase ++ ".eprime-aux"
            , "-out-info"       , stringToText $ outputsDir </> outBase ++ ".eprime-info"
            , "-out-solution"   , stringToText $ outputsDir </> outBase ++ ".eprime-solution"
            ] ++ srOptions srExtraOptions
        stderrSR <- lastStderr
        if not (T.null stderrSR)
            then liftIO $ assertFailure $ T.unpack stderrSR
            else do
                eprimeModel       <- liftIO $ readModelFromFile (outputsDir </> modelPath)
                nbEprimeSolutions <- length . filter ((outBase ++ ".eprime-solution.") `isPrefixOf`)
                                          <$> liftIO (getDirectoryContents outputsDir)
                forM_ (take nbEprimeSolutions allNats) $ \ i -> liftIO $ do
                    let eprimeSolutionPath = outBase ++ ".eprime-solution." ++ paddedNum i
                    eprimeSolution <- readModelFromFile (outputsDir </> eprimeSolutionPath)
                    case ignoreLogs $ runNameGen $ translateSolution eprimeModel param eprimeSolution of
                        Left err -> assertFailure $ renderNormal err
                        Right s  -> do
                            let filename = outputsDir </> outBase ++ "-solution" ++ paddedNum i ++ ".solution"
                            writeFile filename (renderNormal s)


validateSolutionNoParam :: TestDirFiles -> FilePath -> TestTree
validateSolutionNoParam TestDirFiles{..} solutionPath =
    testCase (unwords ["Validating solution:", solutionPath]) $ sh $ do
        essence  <- liftIO $ readModelFromFile essenceFile
        solution <- liftIO $ readModelFromFile (outputsDir </> solutionPath)
        case ignoreLogs (validateSolution essence def solution) of
            Left err -> liftIO $ assertFailure $ renderNormal err
            Right () -> return ()


validateSolutionWithParams :: TestDirFiles -> FilePath -> FilePath -> TestTree
validateSolutionWithParams TestDirFiles{..} paramPath solutionPath =
    testCase (unwords ["Validating solution:", paramPath, solutionPath]) $ sh $ do
        essence  <- liftIO $ readModelFromFile essenceFile
        param    <- liftIO $ readModelFromFile (tBaseDir   </> paramPath)
        solution <- liftIO $ readModelFromFile (outputsDir </> solutionPath)
        case ignoreLogs (validateSolution essence param solution) of
            Left err -> liftIO $ assertFailure $ renderNormal err
            Right () -> return ()

checkExpectedAndExtraFiles :: TestDirFiles -> TestTree
checkExpectedAndExtraFiles TestDirFiles{..} = testCaseSteps "Checking" $ \ step -> do
    let
        relevantFile :: FilePath -> Bool
        relevantFile f = or [ suffix `isSuffixOf` f
                            | suffix <- [".eprime", ".eprime-param", ".solution"]
                            ]
    expecteds <- do
        b <- doesDirectoryExist expectedsDir
        if b
            then S.fromList . filter relevantFile <$> getDirectoryContents expectedsDir
            else return S.empty
    outputs   <- do
        b <- doesDirectoryExist outputsDir
        if b
            then S.fromList . filter relevantFile <$> getDirectoryContents outputsDir
            else return S.empty
    let extras = S.difference outputs expecteds

    step "Checking extra files"
    unless (S.null extras) $ assertFailure $ show $ prettyList id ", " (S.toList extras)

    step "Checking expected files"
    forM_ expecteds $ \ item -> do
        step (unwords ["Checking expected file", item])
        let expectedPath  = expectedsDir </> item
        let generatedPath = outputsDir   </> item
        isFile <- doesFileExist generatedPath
        if isFile
            then do
                e <- readModelFromFile expectedPath
                g <- readModelFromFile generatedPath
                case modelDiff e g of
                    Nothing -> return ()
                    Just msg -> assertFailure $ renderNormal $ "files differ:" <+> msg
            else assertFailure $ "file doesn't exist: " ++ generatedPath

equalNumberOfSolutions :: TestDirFiles -> TestTree
equalNumberOfSolutions TestDirFiles{..} =
    testCase "Checking number of solutions" $ do
        dirShouldExist outputsDir
        models    <- filter (".eprime"       `isSuffixOf`) <$> getDirectoryContents outputsDir
        params    <- filter (".eprime-param" `isSuffixOf`) <$> getDirectoryContents outputsDir
        solutions <- filter (".solution"     `isSuffixOf`) <$> getDirectoryContents outputsDir
        let
            grouped :: [(Maybe String, [(String, Int)])]
            grouped =
                (if null params
                    then
                        [ (Nothing, (model, length $ filter (solnPrefix `isPrefixOf`) solutions))
                        | model' <- models
                        , let model = splitOn "." model' |> head
                        , let solnPrefix = model
                        ]
                    else
                        [ (Just param, (model, length $ filter (solnPrefix `isPrefixOf`) solutions))
                        | model          <- map (head . splitOn ".") models
                        , [model2,param] <- map (splitOn "-" . head . splitOn ".") params
                        , model == model2
                        , let solnPrefix = model ++ "-" ++ param
                        ])
                |> sortBy  (comparing fst)
                |> groupBy ((==) `on` fst)
                |> map (\ grp -> (fst (head grp), map snd grp) )
        let
            differentOnes :: [(Maybe String, [(String, Int)])]
            differentOnes =
                [ this
                | this@(_, modelSols) <- grouped
                , let nbSols = map snd modelSols |> nub
                , length nbSols > 1
                ]

        unless (null differentOnes) $
            assertFailure $ show $ vcat
                [ (maybe
                    id
                    (\ p -> hang ("For parameter" <+> pretty p) 4 )
                    param
                  ) $ vcat [ "Model" <+> pretty model <+> "has" <+> pretty nbSols <+> "solutions."
                           | (model, nbSols) <- modelSols
                           ]
                | (param, modelSols) <- differentOnes
                ]

dirShouldExist :: FilePath -> IO ()
dirShouldExist d = do
    b <- doesDirectoryExist d
    unless b $
        assertFailure $ "dir does not exist: " ++ d


modelAll :: FilePath -> Model -> IO ()
modelAll dir = ignoreLogs . runNameGen . outputModels Config
    { logLevel                   = LogNone
    , verboseTrail               = False
    , logRuleFails               = False
    , logRuleSuccesses           = False
    , logRuleAttempts            = False
    , logChoices                 = False
    , strategyQ                  = PickFirst
    , strategyA                  = PickAll
    , representations            = PickAll
    , representationsFinds       = PickAll
    , representationsGivens      = PickAll
    , representationsAuxiliaries = PickAll
    , representationsQuantifieds = PickAll
    , representationsCuts        = PickAll
    , outputDirectory            = dir
    , channelling                = True
    , limitModels                = Nothing
    , numberingStart             = 1
    , smartFilenames             = True
    }
