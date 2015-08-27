{-# LANGUAGE RecordWildCards #-}

module Conjure.UI.MainHelper ( mainWithArgs ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.UserError ( MonadUserError(..) )
import Conjure.UI ( UI(..) )
import Conjure.UI.IO ( readModelFromFile, readModelPreambleFromFile, writeModel, EssenceFileMode(..) )
import Conjure.UI.Model ( parseStrategy, outputModels )
import qualified Conjure.UI.Model as Config ( Config(..) )
import Conjure.UI.RefineParam ( refineParam )
import Conjure.UI.TranslateSolution ( translateSolution )
import Conjure.UI.ValidateSolution ( validateSolution )
import Conjure.UI.TypeCheck ( typeCheckModel_StandAlone )
import Conjure.UI.LogFollow ( refAnswers )
import Conjure.UI.Split ( outputSplittedModels )
import Conjure.UI.VarSymBreaking ( outputVarSymBreaking )
import Conjure.UI.ParameterGenerator ( parameterGenerator )

import Conjure.Language.NameGen ( runNameGen )
import Conjure.Language.Pretty ( pretty, renderNormal, renderWide )
import Conjure.Language.ModelDiff ( modelDiffIO )
import Conjure.Rules.Definition ( viewAuto, Strategy(..) )
import Conjure.Process.Enumerate ( EnumerateDomain )

-- base
import System.IO ( hSetBuffering, stdout, BufferMode(..) )

-- shelly
import Shelly ( run, lastStderr )

-- text
import qualified Data.Text as T ( null )
    

mainWithArgs :: (MonadIO m, MonadLog m, MonadFail m, MonadUserError m, EnumerateDomain m) => UI -> m ()
mainWithArgs Modelling{..} = do
    model <- readModelFromFile essence
    liftIO $ hSetBuffering stdout NoBuffering
    liftIO $ maybe (return ()) setRandomSeed seed
    case savedChoices of
        Just f  -> refAnswers f
        Nothing -> return ()

    let
        parseStrategy_ s = maybe (userErr1 ("Not a valid strategy:" <+> pretty strategyQ))
                                 return
                                 (parseStrategy s)

    config <- do
        strategyQ'                  <- parseStrategy_ strategyQ
        strategyA'                  <- parseStrategy_ strategyA
        representations'            <- maybe (return strategyA')       parseStrategy_ representations
        representationsFinds'       <- maybe (return representations') parseStrategy_ representationsFinds
        representationsGivens'      <- maybe (return representations') parseStrategy_ representationsGivens
        representationsAuxiliaries' <- maybe (return representations') parseStrategy_ representationsAuxiliaries
        representationsQuantifieds' <- maybe (return representations') parseStrategy_ representationsQuantifieds
        representationsCuts'        <- maybe (return representations') parseStrategy_ representationsCuts

        case fst (viewAuto strategyQ') of
            Compact -> userErr1 "The Compact heuristic isn't supported for questions."
            _       -> return ()

        return Config.Config
            { Config.outputDirectory            = outputDirectory
            , Config.logLevel                   = logLevel
            , Config.verboseTrail               = verboseTrail
            , Config.logRuleFails               = logRuleFails
            , Config.logRuleSuccesses           = logRuleSuccesses
            , Config.logRuleAttempts            = logRuleAttempts
            , Config.logChoices                 = logChoices
            , Config.strategyQ                  = strategyQ'
            , Config.strategyA                  = strategyA'
            , Config.representations            = representations'
            , Config.representationsFinds       = representationsFinds'
            , Config.representationsGivens      = representationsGivens'
            , Config.representationsAuxiliaries = representationsAuxiliaries'
            , Config.representationsQuantifieds = representationsQuantifieds'
            , Config.representationsCuts        = representationsCuts'
            , Config.channelling                = channelling
            , Config.limitModels                = if limitModels == Just 0 then Nothing else limitModels
            , Config.numberingStart             = numberingStart
            , Config.smartFilenames             = smartFilenames
            }
    runNameGen $ outputModels config model
mainWithArgs RefineParam{..} = do
    when (null eprime      ) $ userErr1 "Mandatory field --eprime"
    when (null essenceParam) $ userErr1 "Mandatory field --essence-param"
    let outputFilename = fromMaybe (dropExtension essenceParam ++ ".eprime-param") eprimeParam
    output <- runNameGen $ join $ refineParam
                    <$> readModelPreambleFromFile eprime
                    <*> readModelFromFile essenceParam
    writeModel (if outputBinary then BinaryEssence else PlainEssence)
               (Just outputFilename)
               output
mainWithArgs TranslateSolution{..} = do
    when (null eprime        ) $ userErr1 "Mandatory field --eprime"
    when (null eprimeSolution) $ userErr1 "Mandatory field --eprime-solution"
    output <- runNameGen $ join $ translateSolution
                    <$> readModelPreambleFromFile eprime
                    <*> maybe (return def) readModelFromFile essenceParamO
                    <*> readModelFromFile eprimeSolution
    let outputFilename = fromMaybe (dropExtension eprimeSolution ++ ".solution") essenceSolutionO
    writeModel (if outputBinary then BinaryEssence else PlainEssence)
               (Just outputFilename) output
mainWithArgs ValidateSolution{..} = do
    when (null essence        ) $ userErr1 "Mandatory field --essence"
    when (null essenceSolution) $ userErr1 "Mandatory field --solution"
    join $ validateSolution
        <$> readModelFromFile essence
        <*> maybe (return def) readModelFromFile essenceParamO
        <*> readModelFromFile essenceSolution
mainWithArgs Pretty{..} = do
    model <- readModelFromFile essence
    writeModel (if outputBinary then BinaryEssence else PlainEssence)
               Nothing model
mainWithArgs Diff{..} =
    join $ modelDiffIO
        <$> readModelFromFile file1
        <*> readModelFromFile file2
mainWithArgs TypeCheck{..} =
    void $ runNameGen $ join $ typeCheckModel_StandAlone <$> readModelFromFile essence
mainWithArgs Split{..} = do
    model <- readModelFromFile essence
    outputSplittedModels outputDirectory model
mainWithArgs SymmetryDetection{..} = do
    let jsonFilePath = if null json then essence ++ "-json" else json
    model <- readModelFromFile essence
    outputVarSymBreaking jsonFilePath model
mainWithArgs ParameterGenerator{..} = do
    when (null essenceOut) $ userErr1 "Mandatory field --essence-out"
    model  <- readModelFromFile essence
    output <- parameterGenerator model
    writeModel (if outputBinary then BinaryEssence else PlainEssence)
               (Just essenceOut)
               output
mainWithArgs config@Solve{..} = do eprimes   <- conjuring
                                   solutions <- liftIO $ savileRows eprimes
                                   liftIO $ validating solutions
    where
        conjuring = do
            pp $ "Generating models for" <+> pretty essence
            -- tl;dr: rm -rf outputDirectory
            -- removeDirectoryRecursive gets upset if the dir doesn't exist.
            -- terrible solution: create the dir if it doesn't exists, rm -rf after that.
            liftIO $ createDirectoryIfMissing True outputDirectory >> removeDirectoryRecursive outputDirectory
            let modelling = let savedChoices = def
                            in  Modelling{..}                   -- construct a Modelling UI, copying all relevant fields
                                                                -- from the given Solve UI
            mainWithArgs modelling
            eprimes <- filter (".eprime" `isSuffixOf`) <$> liftIO (getDirectoryContents outputDirectory)
            when (null eprimes) $ bug "Failed to generate models."
            pp $ "Generated models:" <+> vcat (map pretty eprimes)
            pp $ "Saved under:" <+> pretty outputDirectory
            return eprimes

        savileRows eprimes = fmap concat $ sequence $
            case essenceParamO of
                Nothing           -> [ savileRowNoParam    config m              | m <- eprimes ]
                Just essenceParam -> [ savileRowWithParams config m essenceParam | m <- eprimes ]

        validating solutions = sequence_ $
            case essenceParamO of
                Nothing           -> [ validateSolutionNoParam    config sol              | (_, _, sol) <- solutions ]
                Just essenceParam -> [ validateSolutionWithParams config sol essenceParam | (_, _, sol) <- solutions ]


pp :: MonadIO m => Doc -> m ()
pp = liftIO . putStrLn . renderWide


savileRowNoParam :: UI -> FilePath -> IO [(FilePath, FilePath, FilePath)]
savileRowNoParam Solve{..} modelPath = sh $ do
    pp $ hsep ["Savile Row:", pretty essence, pretty modelPath]
    let outBase = dropExtension modelPath
    _stdoutSR <- run "savilerow" $
        [ "-in-eprime"      , stringToText $ outputDirectory </> outBase ++ ".eprime"
        , "-out-minion"     , stringToText $ outputDirectory </> outBase ++ ".eprime-minion"
        , "-out-aux"        , stringToText $ outputDirectory </> outBase ++ ".eprime-aux"
        , "-out-info"       , stringToText $ outputDirectory </> outBase ++ ".eprime-info"
        , "-out-solution"   , stringToText $ outputDirectory </> outBase ++ ".eprime-solution"
        , "-run-solver"
        , "-minion"
        ] ++ map stringToText (words savilerowOptions)
          ++ [ "-solver-options", stringToText minionOptions ]
    stderrSR <- lastStderr
    if not (T.null stderrSR)
        then bug (pretty stderrSR)
        else do
            eprimeModel       <- liftIO $ readModelFromFile (outputDirectory </> modelPath)
            nbEprimeSolutions <- length . filter ((outBase ++ ".eprime-solution.") `isPrefixOf`)
                                      <$> liftIO (getDirectoryContents outputDirectory)
            forM (take nbEprimeSolutions allNats) $ \ i -> liftIO $ do
                let eprimeSolutionPath = outBase ++ ".eprime-solution." ++ paddedNum i
                eprimeSolution <- readModelFromFile (outputDirectory </> eprimeSolutionPath)
                s <- ignoreLogs $ runNameGen $ translateSolution eprimeModel def eprimeSolution
                let filename = outputDirectory </> outBase ++ "-solution" ++ paddedNum i ++ ".solution"
                writeFile filename (renderNormal s)
                return (modelPath, "<no param file>", filename)
savileRowNoParam _ _ = bug "savileRowNoParam"


savileRowWithParams :: UI -> FilePath -> FilePath -> IO [(FilePath, FilePath, FilePath)]
savileRowWithParams Solve{..} modelPath paramPath = sh $ do
        pp $ hsep ["Savile Row:", pretty essence, pretty modelPath, pretty paramPath]
        model       <- liftIO $ readModelFromFile (outputDirectory </> modelPath)
        param       <- liftIO $ readModelFromFile paramPath
        eprimeParam <- liftIO $ ignoreLogs $ runNameGen $ refineParam model param
        let outBase = dropExtension modelPath ++ "-" ++ dropDirs (dropExtension paramPath)
        liftIO $ writeFile (outputDirectory </> outBase ++ ".eprime-param") (renderNormal eprimeParam)
        _stdoutSR <- run "savilerow" $
            [ "-in-eprime"      , stringToText $ outputDirectory </> modelPath
            , "-in-param"       , stringToText $ outputDirectory </> outBase ++ ".eprime-param"
            , "-out-minion"     , stringToText $ outputDirectory </> outBase ++ ".eprime-minion"
            , "-out-aux"        , stringToText $ outputDirectory </> outBase ++ ".eprime-aux"
            , "-out-info"       , stringToText $ outputDirectory </> outBase ++ ".eprime-info"
            , "-out-solution"   , stringToText $ outputDirectory </> outBase ++ ".eprime-solution"
            , "-run-solver"
            , "-minion"
            ] ++ map stringToText (words savilerowOptions)
        stderrSR <- lastStderr
        if not (T.null stderrSR)
            then bug (pretty stderrSR)
            else do
                eprimeModel       <- liftIO $ readModelFromFile (outputDirectory </> modelPath)
                nbEprimeSolutions <- length . filter ((outBase ++ ".eprime-solution.") `isPrefixOf`)
                                          <$> liftIO (getDirectoryContents outputDirectory)
                forM (take nbEprimeSolutions allNats) $ \ i -> liftIO $ do
                    let eprimeSolutionPath = outBase ++ ".eprime-solution." ++ paddedNum i
                    eprimeSolution <- readModelFromFile (outputDirectory </> eprimeSolutionPath)
                    result <- runExceptT $ ignoreLogs $ runNameGen $ translateSolution eprimeModel param eprimeSolution
                    case result of
                        Left err -> bug (pretty err)
                        Right s  -> do
                            let filename = outputDirectory </> outBase ++ "-solution" ++ paddedNum i ++ ".solution"
                            writeFile filename (renderNormal s)
                            return (modelPath, paramPath, filename)
savileRowWithParams _ _ _ = bug "savileRowWithParams"


validateSolutionNoParam :: UI -> FilePath -> IO ()
validateSolutionNoParam Solve{..} solutionPath = do
    pp $ hsep ["Validating solution:", pretty essence, pretty solutionPath]
    essenceM <- readModelFromFile essence
    solution <- readModelFromFile solutionPath
    result   <- runExceptT $ ignoreLogs $ validateSolution essenceM def solution
    case result of
        Left err -> bug err
        Right () -> return ()
validateSolutionNoParam _ _ = bug "validateSolutionNoParam"


validateSolutionWithParams :: UI -> FilePath -> FilePath -> IO ()
validateSolutionWithParams Solve{..} solutionPath paramPath = do
    pp $ hsep ["Validating solution:", pretty essence, pretty solutionPath, pretty paramPath]
    essenceM <- readModelFromFile essence
    param    <- readModelFromFile paramPath
    solution <- readModelFromFile solutionPath
    result   <- runExceptT $ ignoreLogs $ validateSolution essenceM param solution
    case result of
        Left err -> bug err
        Right () -> return ()
validateSolutionWithParams _ _ _ = bug "validateSolutionWithParams"

