{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Conjure.ModelAllSolveAll ( tests, QuickOrSlow(..) ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Pretty
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
import Test.Tasty.Options ( IsOption(..) )

-- shelly
import Shelly ( run, errExit, lastStderr, lastExitCode )

-- text
import qualified Data.Text as T ( isInfixOf, lines, unlines )
import qualified Data.Text.IO as T ( readFile )

-- containers
import qualified Data.Set as S ( fromList, toList, empty, null, difference )

-- Diff
import Data.Algorithm.Diff ( Diff(..), getGroupedDiff )
import Data.Algorithm.DiffOutput ( ppDiff )


srOptions :: String -> [Text]
srOptions srExtraOptions =
    [ "-run-solver"
    , "-minion"
    -- , "-timelimit"      , "1200000"
    -- , "-solver-options" , "-cpulimit 1200"
    , "-all-solutions"
    , "-preprocess"     , "None"
    ] ++ map stringToText (words srExtraOptions)


-- | Which tests are we running?
data QuickOrSlow = Quick | Slow | BothQuickAndSlow
    deriving (Eq, Ord, Typeable)

instance IsOption QuickOrSlow where
    defaultValue = Quick

    parseValue "quick" = return Quick
    parseValue "slow" = return Slow
    parseValue "all" = return BothQuickAndSlow
    parseValue _ = Nothing

    optionName = return "select-tests"
    optionHelp = return "Select which tests to run (quick/slow/all). Default is quick."

data TestKind = TK_Invalid | TK_Slow | TK_Quick
  deriving (Eq, Show)


tests :: IO (QuickOrSlow -> TestTree)
tests = do
    srExtraOptions <- do
        env <- getEnvironment
        return $ fromMaybe "-O0" (lookup "SR_OPTIONS" env)
    putStrLn $ "Using Savile Row options: " ++ unwords (map textToString (srOptions srExtraOptions))
    let baseDir = "tests/exhaustive"
    dirs <- mapM (isTestDir baseDir) =<< getAllDirs baseDir
    let testCases = map (testSingleDir srExtraOptions) (catMaybes dirs)
    return (\ quickOrSlow -> testGroup "exhaustive" (map ($ quickOrSlow) testCases) )


data TestDirFiles = TestDirFiles
    { name           :: String          -- a name for the test case
    , testKind       :: TestKind
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
isTestDir baseDir dir = do
    dirContents <- getDirectoryContents dir
    let testKind
            | "invalid" `elem` dirContents = TK_Invalid
            | "slow"    `elem` dirContents = TK_Slow
            | otherwise                    = TK_Quick
    let essenceFiles = filter (".essence" `isSuffixOf`) dirContents
    case essenceFiles of
        [f] -> Just <$> do
            let params = filter (".param"  `isSuffixOf`) dirContents
            expecteds <- do
                let dirExpected = dir </> "expected"
                isDir <- doesDirectoryExist dirExpected
                if isDir
                    then getDirectoryContents dirExpected
                    else return []
            return TestDirFiles
                { name           = drop (length baseDir + 1) dir
                , tBaseDir       = dir
                , outputsDir     = dir </> "outputs"
                , expectedsDir   = dir </> "expected"
                , essenceFile    = dir </> f
                , paramFiles     = params
                , expectedModels = filter (".eprime"   `isSuffixOf`) expecteds
                , expectedSols   = filter (".solution" `isSuffixOf`) expecteds
                , testKind       = testKind
                }
        _ -> return Nothing


-- the first FilePath is the base directory for the exhaustive tests
-- we know at this point that the second FilePath points to a directory D,
-- which contains + an Essence file D/D.essence
--                + D/*.param files if required
--                + D/expected for the expected output files
testSingleDir :: String -> TestDirFiles -> QuickOrSlow -> TestTree
testSingleDir srExtraOptions t@(TestDirFiles{..}) quickOrSlow =
    if shouldRun
        then
            testGroup name $ concat
                [ [conjuring]
                , savileRows
                , [validating]
                , [checkExpectedAndExtraFiles t]
                , [equalNumberOfSolutions t]
                , [noDuplicateSolutions t]
                ]
        else
            testGroup name [testCase "Skip" (return ())]
    where

        shouldRun
            | testKind == TK_Invalid                                               = False
            | testKind == TK_Quick && quickOrSlow `elem` [Quick, BothQuickAndSlow] = True
            | testKind == TK_Slow  && quickOrSlow `elem` [Slow , BothQuickAndSlow] = True
            | otherwise                                                            = False

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
                then validateSolutionNoParam t expectedSols
                else validateSolutionWithParams t [ ( p
                                                    , [ s | s <- expectedSols
                                                          , dropExtension p `isInfixOf` dropExtension s
                                                          ]
                                                    )
                                                  | p <- paramFiles
                                                  ]


savileRowNoParam :: String -> TestDirFiles -> FilePath -> TestTree
savileRowNoParam srExtraOptions TestDirFiles{..} modelPath =
    testCase (unwords ["Savile Row:", modelPath]) $ do
        let outBase = dropExtension modelPath
        fileShouldExist (outputsDir </> outBase ++ ".eprime")
        (stdoutSR, stderrSR, exitCodeSR) <-
            sh $ errExit False $ do
                stdoutSR <- run "savilerow" $
                    [ "-in-eprime"      , stringToText $ outputsDir </> outBase ++ ".eprime"
                    , "-out-minion"     , stringToText $ outputsDir </> outBase ++ ".eprime-minion"
                    , "-out-aux"        , stringToText $ outputsDir </> outBase ++ ".eprime-aux"
                    , "-out-info"       , stringToText $ outputsDir </> outBase ++ ".eprime-info"
                    , "-out-solution"   , stringToText $ outputsDir </> outBase ++ ".eprime-solution"
                    ] ++ srOptions srExtraOptions
                stderrSR   <- lastStderr
                exitCodeSR <- lastExitCode
                return (stdoutSR, stderrSR, exitCodeSR)
        if
            | exitCodeSR == 0 -> do
                eprimeModel       <- readModelPreambleFromFile (outputsDir </> modelPath)
                nbEprimeSolutions <- length . filter ((outBase ++ ".eprime-solution.") `isPrefixOf`)
                                          <$> getDirectoryContents outputsDir
                forM_ (take nbEprimeSolutions allNats) $ \ i -> do
                    let eprimeSolutionPath = outBase ++ ".eprime-solution." ++ paddedNum 6 '0' i
                    eprimeSolution <- readModelFromFile (outputsDir </> eprimeSolutionPath)
                    s <- ignoreLogs $ runNameGen $ translateSolution eprimeModel def eprimeSolution
                    let filename = outputsDir </> outBase ++ "-solution" ++ paddedNum 6 '0' i ++ ".solution"
                    writeFile filename (renderNormal s)
            | T.isInfixOf "where false" (T.unlines [stdoutSR, stderrSR]) -> return ()
            | otherwise -> assertFailure $ renderNormal $ vcat [ "Savile Row stdout:"    <+> pretty stdoutSR
                                                               , "Savile Row stderr:"    <+> pretty stderrSR
                                                               , "Savile Row exit-code:" <+> pretty exitCodeSR
                                                               ]


savileRowWithParams :: String -> TestDirFiles -> FilePath -> FilePath -> TestTree
savileRowWithParams srExtraOptions TestDirFiles{..} modelPath paramPath =
    testCase (unwords ["Savile Row:", modelPath, paramPath]) $ do
        fileShouldExist (outputsDir </> modelPath)
        fileShouldExist (tBaseDir   </> paramPath)
        eprimeModel <- readModelPreambleFromFile (outputsDir </> modelPath)
        param       <- readModelFromFile (tBaseDir   </> paramPath)
        eprimeParam <- ignoreLogs $ runNameGen $ refineParam eprimeModel param
        let outBase = dropExtension modelPath ++ "-" ++ dropExtension paramPath
        writeFile (outputsDir </> outBase ++ ".eprime-param") (renderNormal eprimeParam)
        (stdoutSR, stderrSR, exitCodeSR) <-
            sh $ errExit False $ do
                stdoutSR <- run "savilerow" $
                    [ "-in-eprime"      , stringToText $ outputsDir </> modelPath
                    , "-in-param"       , stringToText $ outputsDir </> outBase ++ ".eprime-param"
                    , "-out-minion"     , stringToText $ outputsDir </> outBase ++ ".eprime-minion"
                    , "-out-aux"        , stringToText $ outputsDir </> outBase ++ ".eprime-aux"
                    , "-out-info"       , stringToText $ outputsDir </> outBase ++ ".eprime-info"
                    , "-out-solution"   , stringToText $ outputsDir </> outBase ++ ".eprime-solution"
                    ] ++ srOptions srExtraOptions
                stderrSR   <- lastStderr
                exitCodeSR <- lastExitCode
                return (stdoutSR, stderrSR, exitCodeSR)
        if
            | exitCodeSR == 0 -> do
                nbEprimeSolutions <- length . filter ((outBase ++ ".eprime-solution.") `isPrefixOf`)
                                          <$> getDirectoryContents outputsDir
                forM_ (take nbEprimeSolutions allNats) $ \ i -> do
                    let eprimeSolutionPath = outBase ++ ".eprime-solution." ++ paddedNum 6 '0' i
                    eprimeSolution <- readModelFromFile (outputsDir </> eprimeSolutionPath)
                    res <- runExceptT $ ignoreLogs $ runNameGen $ translateSolution eprimeModel param eprimeSolution
                    case res of
                        Left err -> assertFailure $ renderNormal err
                        Right s  -> do
                            let filename = outputsDir </> outBase ++ "-solution" ++ paddedNum 6 '0' i ++ ".solution"
                            writeFile filename (renderNormal s)
            | T.isInfixOf "where false" (T.unlines [stdoutSR, stderrSR]) -> return ()
            | otherwise -> assertFailure $ renderNormal $ vcat [ "Savile Row stdout:"    <+> pretty stdoutSR
                                                               , "Savile Row stderr:"    <+> pretty stderrSR
                                                               , "Savile Row exit-code:" <+> pretty exitCodeSR
                                                               ]


validateSolutionNoParam :: TestDirFiles -> [FilePath] -> TestTree
validateSolutionNoParam TestDirFiles{..} solutionPaths =
    testCaseSteps "Validating solutions" $ \ step -> do
        essence <- readModelFromFile essenceFile
        forM_ solutionPaths $ \ solutionPath -> do
            step (unwords ["Validating solution:", solutionPath])
            fileShouldExist (outputsDir </> solutionPath)
            solution <- readModelFromFile (outputsDir </> solutionPath)
            result   <- runExceptT $ ignoreLogs $ validateSolution essence def solution
            case result of
                Left err -> assertFailure $ renderNormal err
                Right () -> return ()


validateSolutionWithParams :: TestDirFiles -> [(FilePath, [FilePath])] -> TestTree
validateSolutionWithParams TestDirFiles{..} paramSolutionPaths =
    testCaseSteps "Validating solutions" $ \ step -> do
        essence <- readModelFromFile essenceFile
        forM_ paramSolutionPaths $ \ (paramPath, solutionPaths) -> do
            fileShouldExist (tBaseDir </> paramPath)
            param <- readModelFromFile (tBaseDir </> paramPath)
            forM_ solutionPaths $ \ solutionPath -> do
                step (unwords ["Validating solution:", paramPath, solutionPath])
                fileShouldExist (outputsDir </> solutionPath)
                solution <- readModelFromFile (outputsDir </> solutionPath)
                result   <- runExceptT $ ignoreLogs $ runNameGen $ validateSolution essence param solution
                case result of
                    Left err -> assertFailure $ renderNormal err
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
        step (unwords ["Checking expected file:", item])
        let expectedPath  = expectedsDir </> item
        let generatedPath = outputsDir   </> item
        isFile <- doesFileExist generatedPath
        if isFile
            then do
                e <- T.lines <$> T.readFile expectedPath
                g <- takeWhile (/= "$ Conjure's") . T.lines <$> T.readFile generatedPath
                let
                    fmapDiff f (First x) = First (f x)
                    fmapDiff f (Second x) = Second (f x)
                    fmapDiff f (Both x y) = Both (f x) (f y)

                    isBoth Both{} = True
                    isBoth _ = False

                    diffs = filter (not . isBoth) $ getGroupedDiff e g
                    diffsString = fmap (fmapDiff (fmap textToString)) diffs

                if null diffs
                    then return ()
                    else assertFailure $ renderNormal $ vcat ["files differ.", pretty (ppDiff diffsString)]
            else assertFailure $ "file doesn't exist: " ++ generatedPath


equalNumberOfSolutions :: TestDirFiles -> TestTree
equalNumberOfSolutions TestDirFiles{..} =
    testCase "Checking number of solutions" $ do
        dirShouldExist outputsDir
        models    <- filter (".eprime"       `isSuffixOf`) <$> getDirectoryContents outputsDir
        params    <- filter (".eprime-param" `isSuffixOf`) <$> getDirectoryContents outputsDir
        solutions <- filter (".solution"     `isSuffixOf`) <$> getDirectoryContents outputsDir
        let
            grouped :: [ ( Maybe String             -- the parameter
                         , [(String, Int)]          -- model, number of solutions
                         ) ]
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
                [ maybe
                    id
                    (\ p -> hang ("For parameter" <+> pretty p) 4 )
                    param
                    $ vcat [ "Model" <+> pretty model <+> "has" <+> pretty nbSols <+> "solutions."
                           | (model, nbSols) <- modelSols
                           ]
                | (param, modelSols) <- differentOnes
                ]


noDuplicateSolutions :: TestDirFiles -> TestTree
noDuplicateSolutions TestDirFiles{..} =
    testCase "Checking duplicate solutions" $ do
        dirShouldExist outputsDir
        models    <- filter (".eprime"       `isSuffixOf`) <$> getDirectoryContents outputsDir
        params    <- filter (".eprime-param" `isSuffixOf`) <$> getDirectoryContents outputsDir
        solutions <- filter (".solution"     `isSuffixOf`) <$> getDirectoryContents outputsDir
        solutionContents <- forM solutions $ \ s -> do m <- readModelFromFile (outputsDir </> s)
                                                       return (s, m)
        let
            grouped :: [ ( Maybe String             -- the parameter
                         , [(String, [String])]     -- model, duplicate solutions
                         ) ]
            grouped =
                (if null params
                    then
                        [ (Nothing, (model, duplicateSolutions))
                        | model' <- models
                        , let model = splitOn "." model' |> head
                        , let solnPrefix = model
                        , let thisSolutions = filter ((solnPrefix `isPrefixOf`) . fst) solutionContents
                        , let duplicateSolutions =
                                [ s1name
                                | (s1name, s1) <- thisSolutions
                                , not $ null [ s2name | (s2name, s2) <- thisSolutions
                                             , s1name /= s2name && s1 == s2
                                             ]
                                ]
                        , not (null duplicateSolutions)
                        ]
                    else
                        [ (Just param, (model, duplicateSolutions))
                        | model          <- map (head . splitOn ".") models
                        , [model2,param] <- map (splitOn "-" . head . splitOn ".") params
                        , model == model2
                        , let solnPrefix = model ++ "-" ++ param
                        , let thisSolutions = filter ((solnPrefix `isPrefixOf`) . fst) solutionContents
                        , let duplicateSolutions =
                                [ s1name
                                | (s1name, s1) <- thisSolutions
                                , not $ null [ s2name | (s2name, s2) <- thisSolutions
                                             , s1name /= s2name && s1 == s2
                                             ]
                                ]
                        , not (null duplicateSolutions)
                        ])
                |> sortBy  (comparing fst)
                |> groupBy ((==) `on` fst)
                |> map (\ grp -> (fst (head grp), map snd grp) )

        unless (null grouped) $
            assertFailure $ show $ vcat
                [ case param of
                    Nothing -> "For model" <+> pretty model <++> rest
                    Just p  -> "For parameter" <+> pretty p <+> ", for model" <+> pretty model <++> rest
                | (param, duplicateSols) <- grouped
                , (model, sols) <- duplicateSols
                , let rest = "Duplicate solutions:" <++> prettyList id "," sols
                ]


dirShouldExist :: FilePath -> IO ()
dirShouldExist d = do
    b <- doesDirectoryExist d
    unless b $
        assertFailure $ "dir does not exist: " ++ d

fileShouldExist :: FilePath -> IO ()
fileShouldExist f = do
    b <- doesFileExist f
    unless b $
        assertFailure $ "file does not exist: " ++ f


modelAll :: FilePath -> Model -> IO ()
modelAll dir = ignoreLogs . runNameGen . outputModels Config
    { logLevel                   = LogNone
    , verboseTrail               = False
    , rewritesTrail              = False
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
    , representationLevels       = True
    , limitModels                = Nothing
    , numberingStart             = 1
    , smartFilenames             = True
    }
