{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.UI.MainHelper ( mainWithArgs, savilerowScriptName ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.UserError
import Conjure.UI ( UI(..), OutputFormat(..) )
import Conjure.UI.IO ( readModel, readModelFromFile, readModelFromStdin
                     , readModelInfoFromFile, readParamOrSolutionFromFile
                     , writeModel )
import Conjure.UI.Model ( parseStrategy, outputModels, modelRepresentationsJSON )
import qualified Conjure.UI.Model as Config ( Config(..) )
import Conjure.UI.TranslateParameter ( translateParameter )
import Conjure.UI.TranslateSolution ( translateSolution )
import Conjure.UI.ValidateSolution ( validateSolution )
import Conjure.UI.TypeCheck ( typeCheckModel_StandAlone )
import Conjure.UI.Split ( outputSplittedModels, removeUnusedDecls )
import Conjure.UI.VarSymBreaking ( outputVarSymBreaking )
import Conjure.UI.ParameterGenerator ( parameterGenerator )
import Conjure.UI.NormaliseQuantified ( normaliseQuantifiedVariables )
import Conjure.UI.TypeScript ( tsDef )

import Conjure.Language.Name ( Name(..) )
import Conjure.Language.Definition ( Model(..), Statement(..), Declaration(..), FindOrGiven(..) )
import Conjure.Language.Type ( TypeCheckerMode(..) )
import Conjure.Language.Domain ( Domain(..), Range(..) )
import Conjure.Language.NameGen ( NameGenM, runNameGen )
import Conjure.Language.Pretty ( pretty, prettyList, renderNormal, render, prParens )
import qualified Conjure.Language.ParserC as ParserC ( parseModel )
import Conjure.Language.ModelDiff ( modelDiffIO )
import Conjure.Rules.Definition ( viewAuto, Strategy(..) )
import Conjure.Process.Enumerate ( EnumerateDomain )
import Conjure.Process.ModelStrengthening ( strengthenModel )
import Conjure.Language.NameResolution ( resolveNamesMulti )
import Conjure.Language.ModelStats ( modelDeclarationsJSON )

-- base
import System.IO ( Handle, hSetBuffering, stdout, BufferMode(..) )
import System.Environment ( getEnvironment )
import System.Info ( os )
import GHC.Conc ( numCapabilities )
import GHC.IO.Handle ( hIsEOF, hClose, hGetLine )
import Data.Char ( isDigit )

-- containers
import qualified Data.Set as S

-- filepath
import System.FilePath ( splitFileName, takeBaseName, (<.>) )

-- system-filepath
import qualified Filesystem.Path as Sys ( FilePath )

-- directory
import System.Directory ( copyFile, findExecutable )

-- shelly
import Shelly ( runHandle, lastStderr, lastExitCode, errExit, Sh )

-- text
import qualified Data.Text as T ( unlines, isInfixOf )

-- parallel-io
import Control.Concurrent.ParallelIO.Global ( parallel, parallel_, stopGlobalPool )


mainWithArgs :: forall m .
    MonadIO m =>
    MonadLog m =>
    MonadFail m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    UI -> m ()
mainWithArgs TSDEF{} = liftIO tsDef
mainWithArgs mode@Modelling{..} = do
    essenceM <- readModelFromFile essence
    doIfNotCached          -- start the show!
        ( sort (mStatements essenceM)
        , portfolio
        -- when the following flags change, invalidate hash
        -- nested tuples, because :(
        , ( numberingStart
          , smartFilenames
          , strategyQ
          , strategyA
          , responses
          )
        , ( representations
          , representationsFinds
          , representationsGivens
          , representationsAuxiliaries
          , representationsQuantifieds
          , representationsCuts
          )
        , ( channelling
          , representationLevels
          , seed
          , limitModels
          , limitTime
          , outputFormat
          )
        )
        (outputDirectory </> ".conjure-checksum")
        (pp logLevel "Using cached models.")
        (void $ mainWithArgs_Modelling "" mode Nothing S.empty)
mainWithArgs TranslateParameter{..} = do
    when (null eprime      ) $ userErr1 "Mandatory field --eprime"
    when (null essenceParam) $ userErr1 "Mandatory field --essence-param"
    let outputFilename = fromMaybe (dropExtension essenceParam ++ ".eprime-param") eprimeParam
    eprimeF <- readModelInfoFromFile eprime
    essenceParamF <- readParamOrSolutionFromFile essenceParam
    output <- runNameGen () $ translateParameter eprimeF essenceParamF
    writeModel lineWidth outputFormat (Just outputFilename) output
mainWithArgs TranslateSolution{..} = do
    when (null eprime        ) $ userErr1 "Mandatory field --eprime"
    when (null eprimeSolution) $ userErr1 "Mandatory field --eprime-solution"
    eprimeF <- readModelInfoFromFile eprime
    essenceParamF <- maybe (return def) readParamOrSolutionFromFile essenceParamO
    eprimeSolutionF <- readParamOrSolutionFromFile eprimeSolution
    output <- runNameGen () $ translateSolution eprimeF essenceParamF eprimeSolutionF
    let outputFilename = fromMaybe (dropExtension eprimeSolution ++ ".solution") essenceSolutionO
    writeModel lineWidth outputFormat (Just outputFilename) output
mainWithArgs ValidateSolution{..} = do
    when (null essence        ) $ userErr1 "Mandatory field --essence"
    when (null essenceSolution) $ userErr1 "Mandatory field --solution"
    essence2  <- readModelFromFile essence
    param2    <- maybe (return def) readParamOrSolutionFromFile essenceParamO
    solution2 <- readParamOrSolutionFromFile essenceSolution
    [essence3, param3, solution3] <- runNameGen () $ resolveNamesMulti [essence2, param2, solution2]
    runNameGen () $ validateSolution essence3 param3 solution3
mainWithArgs IDE{..} = do
    essence2 <-
        if null essence
            then readModelFromStdin
            else readModelFromFile essence
    void $ runNameGen () $ typeCheckModel_StandAlone essence2
    if
        | dumpDeclarations    -> liftIO $ putStrLn $ render lineWidth (modelDeclarationsJSON essence2)
        | dumpRepresentations -> do
            json <- runNameGen () $ modelRepresentationsJSON essence2
            liftIO $ putStrLn $ render lineWidth json
        | otherwise           -> writeModel lineWidth JSON Nothing essence2
mainWithArgs Pretty{..} = do
    model0 <- if or [ s `isSuffixOf` essence
                    | s <- [".param", ".eprime-param", ".solution", ".eprime.solution"] ]
                then do
                    liftIO $ putStrLn "Parsing as a parameter file"
                    readParamOrSolutionFromFile essence
                else readModelFromFile essence
    let model1 = model0
                    |> (if normaliseQuantified then normaliseQuantifiedVariables else id)
                    |> (if removeUnused then removeUnusedDecls else id)
    writeModel lineWidth outputFormat Nothing model1
mainWithArgs Diff{..} =
    join $ modelDiffIO
        <$> readModelFromFile file1
        <*> readModelFromFile file2
mainWithArgs TypeCheck{..} = do
    essenceF <- readModelFromFile essence
    void $ runNameGen () $ typeCheckModel_StandAlone essenceF
mainWithArgs Split{..} = do
    model <- readModelFromFile essence
    outputSplittedModels outputDirectory model
mainWithArgs SymmetryDetection{..} = do
    let jsonFilePath = if null json then essence ++ "-json" else json
    model <- readModelFromFile essence
    outputVarSymBreaking jsonFilePath model
mainWithArgs ParameterGenerator{..} = do
    when (null essenceOut) $ userErr1 "Mandatory field --essence-out"
    model <- readModelFromFile essence
    (output, classes) <- runNameGen () $ parameterGenerator minInt maxInt model
    writeModel lineWidth outputFormat (Just essenceOut) output
    let
        toIrace nm lb ub _ | lb == ub =
            pretty nm <+>
            "\"-" <> pretty nm <> " \" c" <+>
            prParens (pretty lb)
        toIrace nm lb ub (Just klass) =
            pretty nm <+>
            "\"-" <> pretty nm <> " \" " <> pretty klass <+>
            prettyList prParens "," [lb, ub]
        toIrace nm _ _ Nothing = bug ("Missing class for:" <+> pretty nm)

        essenceOutFileContents = render lineWidth $ vcat
            [ toIrace nm lb ub (lookup nm classes)
            | Declaration (FindOrGiven Given nm (DomainInt _ [RangeBounded lb ub])) <- mStatements output
            ]
    liftIO $ writeFile (essenceOut ++ ".irace") (essenceOutFileContents ++ "\n")
mainWithArgs ModelStrengthening{..} =
    readModelFromFile essence >>=
      strengthenModel logLevel logRuleSuccesses >>=
        writeModel lineWidth outputFormat (Just essenceOut)
mainWithArgs config@Solve{..} = do
    let executables = [ ( "minion"          , "minion" )
                      , ( "gecode"          , "fzn-gecode" )
                      , ( "chuffed"         , "fzn-chuffed" )
                      , ( "cadical"         , "cadical" )
                      , ( "glucose"         , "glucose" )
                      , ( "glucose-syrup"   , "glucose-syrup" )
                      , ( "lingeling"       , "lingeling" )
                      , ( "plingeling"      , "plingeling" )
                      , ( "treengeling"     , "treengeling" )
                      , ( "minisat"         , "minisat" )
                      , ( "bc_minisat_all"  , "bc_minisat_all_release" )
                      , ( "nbc_minisat_all" , "nbc_minisat_all_release" )
                      , ( "open-wbo"        , "open-wbo" )
                      , ( "coin-or"         , "minizinc" )
                      , ( "cplex"           , "minizinc" )
                      ]
    -- some sanity checks
    case lookup solver executables of
        Nothing -> userErr1 ("Unsupported solver:" <+> pretty solver)
        Just ex -> do
            fp <- liftIO $ findExecutable ex
            case fp of
                Nothing -> userErr1 ("Cannot find executable" <+> pretty ex <+> "(for solver" <+> pretty solver <> ")")
                Just _  -> return ()
    case solver of
        "cplex" -> do
            env <- liftIO getEnvironment
            case lookup "CPLEX_PATH" env of
                Nothing -> userErr1 $ vcat
                    [ "Set environment variable CPLEX_PATH. Something like:"
                    , "    CPLEX_PATH=/path/to/cplex/library conjure solve"
                    ]
                Just{} -> return ()
        _ -> return ()
    unless (nbSolutions == "all" || all isDigit nbSolutions) $
        userErr1 (vcat [ "The value for --number-of-solutions must either be a number or the string \"all\"."
                       , "Was given:" <+> pretty nbSolutions
                       ])
    when (solver `elem` ["bc_minisat_all", "nbc_minisat_all"] && nbSolutions /= "all") $
        userErr1 $ "The solvers bc_minisat_all and nbc_minisat_all only work with --number-of-solutions=all"
    essenceM <- readModelFromFile essence
    unless (null [ () | Objective{} <- mStatements essenceM ]) $ do -- this is an optimisation problem
        when (nbSolutions == "all" || nbSolutions /= "1") $
            userErr1 ("Not supported for optimisation problems: --number-of-solutions=" <> pretty nbSolutions)
    essenceParamsParsed <- forM essenceParams $ \ f -> do
        p <- readParamOrSolutionFromFile f
        return (f, p)
    let givens = [ nm | Declaration (FindOrGiven Given nm _) <- mStatements essenceM ]
              ++ [ nm | Declaration (GivenDomainDefnEnum nm) <- mStatements essenceM ]
    when (not (null givens) && null essenceParams) $
        userErr1 $ vcat
            [ "The problem specification is parameterised, but no *.param files are given."
            , "Parameters:" <+> prettyList id "," givens
            ]
    let isEmptyParam Model{mStatements=[]} = True
        isEmptyParam _ = False
        hasNonEmptyParams =
            null essenceParams ||                               -- either no params were given
            all (isEmptyParam . snd) essenceParamsParsed        -- or all those given were empty
    when (null givens && not hasNonEmptyParams) $
        userErr1 "The problem specification is _not_ parameterised, but *.param files are given."

    eprimes <-
        if not (null useExistingModels)
            then do
                pp logLevel "Using existing models."
                allEprimes <- getEprimes
                let missingModels = useExistingModels \\ allEprimes
                if null missingModels
                    then return useExistingModels
                    else userErr1 $ "Models not found:" <+> vcat (map pretty missingModels)
            else doIfNotCached          -- start the show!
                    ( sort (mStatements essenceM)
                    , portfolio
                    -- when the following flags change, invalidate hash
                    -- nested tuples, because :(
                    , ( numberingStart
                      , smartFilenames
                      , strategyQ
                      , strategyA
                      , responses
                      )
                    , ( representations
                      , representationsFinds
                      , representationsGivens
                      , representationsAuxiliaries
                      , representationsQuantifieds
                      , representationsCuts
                      )
                    , ( channelling
                      , representationLevels
                      , seed
                      , limitModels
                      , limitTime
                      , outputFormat
                      )
                    )
                    (outputDirectory </> ".conjure-checksum")
                    (pp logLevel "Using cached models." >> getEprimes)
                    conjuring

    eprimesParsed <- forM eprimes $ \ f -> do
        p <- readModelInfoFromFile (outputDirectory </> f)
        return (f, p)

    msolutions <- liftIO $ savileRows eprimesParsed essenceParamsParsed
    case msolutions of
        Left msg        -> userErr msg
        Right []        -> pp logLevel "No solutions found."
        Right solutions -> do
            when validateSolutionsOpt $ liftIO $ validating solutions
            when copySolutions $ do
                -- clean-up: move the solutions next to the essence file.
                -- our intention is that a user intending to just solve something
                -- should never have to look into outputDirectory.
                let
                    copySolution :: MonadIO m => FilePath -> FilePath -> m ()
                    copySolution old new = do
                        pp logLevel ("Copying solution to:" <+> pretty new)
                        liftIO (copyFile old new)
                let (essenceDir0, essenceFilename) = splitFileName essence
                let essenceDir = if essenceDir0 == "./" then "" else essenceDir0
                let essenceBasename = takeBaseName essenceFilename
                when (length eprimes == 1) $
                    if null essenceParams
                        then do
                            let solutions' = [ solution
                                             | (_, _, Just solution) <- solutions ]
                            case solutions' of
                                [solution] ->
                                    copySolution solution (essenceDir
                                                            </> intercalate "-" [ essenceBasename
                                                                                ]
                                                            <.> ".solution")
                                _ -> forM_ (zip allNats solutions') $ \ (i, solution) ->
                                    copySolution solution (essenceDir
                                                            </> intercalate "-" [ essenceBasename
                                                                                , padLeft 6 '0' (show i)
                                                                                ]
                                                            <.> ".solution")
                        else forM_ essenceParams $ \ essenceParam -> do
                            let (_paramDir, paramFilename) = splitFileName essenceParam
                            let paramBasename = takeBaseName paramFilename
                            let solutions' = [ solution
                                             | (_, essenceParam', Just solution) <- solutions
                                             , essenceParam == essenceParam' ]
                            case solutions' of
                                [solution] ->
                                    copySolution solution (essenceDir
                                                            </> intercalate "-" [ essenceBasename
                                                                                , paramBasename
                                                                                ]
                                                            <.> ".solution")
                                _ -> forM_ (zip allNats solutions') $ \ (i, solution) ->
                                    copySolution solution (essenceDir
                                                            </> intercalate "-" [ essenceBasename
                                                                                , paramBasename
                                                                                , padLeft 6 '0' (show i)
                                                                                ]
                                                            <.> ".solution")

    liftIO stopGlobalPool

    where
        conjuring :: m [FilePath]
        conjuring = do
            pp logLevel $ "Generating models for" <+> pretty essence
            liftIO $ removeDirectoryIfExists outputDirectory
            let modelling = let savedChoices = def
                                estimateNumberOfModels = False
                            in  Modelling{..}                   -- construct a Modelling UI, copying all relevant fields
                                                                -- from the given Solve UI
            n <- mainWithArgs_Modelling "" modelling Nothing S.empty
            eprimes <- getEprimes
            when (null eprimes) $ bug "Failed to generate models."
            if (S.size n == 1)
                then pp logLevel $ "Generated models:" <+> prettyList id "," eprimes
                else pp logLevel $ "Generated" <+> pretty (S.size n) <+> "models:" <+> prettyList id "," eprimes
            pp logLevel $ "Generated" <+> pretty (S.size n) <+> "models:" <+> prettyList id "," eprimes
            pp logLevel $ "Saved under:" <+> pretty outputDirectory
            return eprimes

        getEprimes :: m [FilePath]
        getEprimes = sort . filter (".eprime" `isSuffixOf`) <$> liftIO (getDirectoryContents outputDirectory)

        combineResults :: [Either e [a]] -> Either e [a]
        combineResults = fmap concat . sequence

        savileRows
            :: [(FilePath, Model)]      -- models
            -> [(FilePath, Model)]      -- params
            -> IO (Either [Doc] [(FilePath, FilePath, Maybe FilePath)])
        savileRows eprimes params = fmap combineResults $
            if null params
                then autoParallel [ savileRowNoParam config m
                                  | m <- eprimes
                                  ]
                else autoParallel [ savileRowWithParams config m p
                                  | m <- eprimes
                                  , p <- params
                                  ]

        validating :: [(FilePath, FilePath, Maybe FilePath)] -> IO ()
        validating solutions =
            if null essenceParams
                then autoParallel_ [ validateSolutionNoParam config sol
                                   | (_, _, Just sol) <- solutions ]
                else autoParallel_ [ validateSolutionWithParams config sol p
                                   | (_, p, Just sol) <- solutions ]


mainWithArgs_Modelling :: forall m .
    MonadIO m =>
    MonadLog m =>
    MonadFail m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    String ->                   -- modelNamePrefix
    UI ->
    Maybe Int ->                -- portfolioSize for the recursive call
    S.Set Int ->                -- modelHashesBefore
    m (S.Set Int)
mainWithArgs_Modelling _ mode@Modelling{..} _ modelHashesBefore | Just portfolioSize <- portfolio = do
    pp logLevel $ "Running in portfolio mode, aiming to generate" <+> pretty portfolioSize <+> "models."
    let
        go modelsSoFar [] = do
            pp logLevel $ "Done, no more levels, generated" <+> pretty (S.size modelsSoFar) <+> "models."
            return modelsSoFar
        go modelsSoFar (l:ls) = do
            let nbModelsNeeded = portfolioSize - S.size modelsSoFar
            if nbModelsNeeded <= 0
                then return modelsSoFar
                else do
                    modelsSoFar' <- l (Just portfolioSize) modelsSoFar
                    go modelsSoFar' ls

    go modelHashesBefore
        [ mainWithArgs_Modelling "01_compact"
            mode { portfolio = Nothing
                 , strategyA = "c"
                 , representations = Just "c"
                 , representationsFinds = Just "c"
                 , representationsGivens = Just "c"
                 , representationsAuxiliaries = Just "c"
                 , representationsQuantifieds = Just "c"
                 , representationsCuts = Just "c"
                 , channelling = False
                 , representationLevels = True
                 , smartFilenames = True
                 }
        , mainWithArgs_Modelling "02_sparse"
            mode { portfolio = Nothing
                 , strategyA = "s"
                 , representations = Just "s"
                 , representationsFinds = Just "s"
                 , representationsGivens = Just "s"
                 , representationsAuxiliaries = Just "s"
                 , representationsQuantifieds = Just "s"
                 , representationsCuts = Just "s"
                 , channelling = False
                 , representationLevels = True
                 , smartFilenames = True
                 }
        , mainWithArgs_Modelling "03_nochPrunedLevels"
            mode { portfolio = Nothing
                 , strategyA = "x"
                 , representations = Just "x"
                 , representationsFinds = Just "x"
                 , representationsGivens = Just "s"
                 , representationsAuxiliaries = Just "c"
                 , representationsQuantifieds = Just "c"
                 , representationsCuts = Just "c"
                 , channelling = False
                 , representationLevels = True
                 , smartFilenames = True
                 }
        , mainWithArgs_Modelling "04_nochAllLevels"
            mode { portfolio = Nothing
                 , strategyA = "x"
                 , representations = Just "x"
                 , representationsFinds = Just "x"
                 , representationsGivens = Just "s"
                 , representationsAuxiliaries = Just "c"
                 , representationsQuantifieds = Just "c"
                 , representationsCuts = Just "c"
                 , channelling = False
                 , representationLevels = False
                 , smartFilenames = True
                 }
        , mainWithArgs_Modelling "05_chPrunedLevels"
            mode { portfolio = Nothing
                 , strategyA = "x"
                 , representations = Just "x"
                 , representationsFinds = Just "x"
                 , representationsGivens = Just "s"
                 , representationsAuxiliaries = Just "c"
                 , representationsQuantifieds = Just "c"
                 , representationsCuts = Just "c"
                 , channelling = True
                 , representationLevels = True
                 , smartFilenames = True
                 }
        , mainWithArgs_Modelling "06_chAllLevels"
            mode { portfolio = Nothing
                 , strategyA = "x"
                 , representations = Just "x"
                 , representationsFinds = Just "x"
                 , representationsGivens = Just "s"
                 , representationsAuxiliaries = Just "c"
                 , representationsQuantifieds = Just "c"
                 , representationsCuts = Just "c"
                 , channelling = True
                 , representationLevels = False
                 , smartFilenames = True
                 }
        , mainWithArgs_Modelling "07_fullPrunedLevels"
            mode { portfolio = Nothing
                 , strategyA = "x"
                 , representations = Just "x"
                 , representationsFinds = Just "x"
                 , representationsGivens = Just "s"
                 , representationsAuxiliaries = Just "x"
                 , representationsQuantifieds = Just "x"
                 , representationsCuts = Just "x"
                 , channelling = True
                 , representationLevels = True
                 , smartFilenames = True
                 }
        , mainWithArgs_Modelling "08_fullAllLevels"
            mode { portfolio = Nothing
                 , strategyA = "x"
                 , representations = Just "x"
                 , representationsFinds = Just "x"
                 , representationsGivens = Just "s"
                 , representationsAuxiliaries = Just "x"
                 , representationsQuantifieds = Just "x"
                 , representationsCuts = Just "x"
                 , channelling = True
                 , representationLevels = False
                 , smartFilenames = True
                 }
        , mainWithArgs_Modelling "09_fullParamsPrunedLevels"
            mode { portfolio = Nothing
                 , strategyA = "x"
                 , representations = Just "x"
                 , representationsFinds = Just "x"
                 , representationsGivens = Just "x"
                 , representationsAuxiliaries = Just "x"
                 , representationsQuantifieds = Just "x"
                 , representationsCuts = Just "x"
                 , channelling = True
                 , representationLevels = True
                 , smartFilenames = True
                 }
        , mainWithArgs_Modelling "10_fullParamsAllLevels"
            mode { portfolio = Nothing
                 , strategyA = "x"
                 , representations = Just "x"
                 , representationsFinds = Just "x"
                 , representationsGivens = Just "x"
                 , representationsAuxiliaries = Just "x"
                 , representationsQuantifieds = Just "x"
                 , representationsCuts = Just "x"
                 , channelling = True
                 , representationLevels = False
                 , smartFilenames = True
                 }
        ]
mainWithArgs_Modelling "" mode portfolioSize modelHashesBefore =
    mainWithArgs_Modelling "model" mode portfolioSize modelHashesBefore
mainWithArgs_Modelling modelNamePrefix Modelling{..} portfolioSize modelHashesBefore = do
    unless (modelNamePrefix == "model") $
        pp logLevel $ "Portfolio level:" <+> pretty modelNamePrefix
    let
        parseStrategy_ s = maybe (userErr1 ("Not a valid strategy:" <+> pretty s))
                                 return
                                 (parseStrategy s)

        getConfig = do
            strategyQ'                  <- parseStrategy_ strategyQ
            strategyA'                  <- parseStrategy_ strategyA
            representations'            <- maybe (return strategyA')       parseStrategy_ representations
            representationsFinds'       <- maybe (return representations') parseStrategy_ representationsFinds
            representationsGivens'      <- maybe (return Sparse          ) parseStrategy_ representationsGivens
            representationsAuxiliaries' <- maybe (return representations') parseStrategy_ representationsAuxiliaries
            representationsQuantifieds' <- maybe (return representations') parseStrategy_ representationsQuantifieds
            representationsCuts'        <- maybe (return representations') parseStrategy_ representationsCuts

            case fst (viewAuto strategyQ') of
                Compact -> userErr1 "The Compact heuristic isn't supported for questions."
                _       -> return ()

            let
                parseCommaSeparated :: Read a => Doc -> String -> m (Maybe [a])
                parseCommaSeparated flag str =
                    if null str
                        then return Nothing
                        else do
                            let parts = splitOn "," str
                            let intParts = mapMaybe readMay parts
                            if length parts == length intParts
                                then return (Just intParts)
                                else userErr1 $ vcat [ "Cannot parse the value for" <+> flag
                                                     , "Expected a comma separated list of integers."
                                                     , "But got:" <+> pretty str
                                                     ]

            responsesList <- parseCommaSeparated "--responses" responses

            responsesRepresentationList <- do
                if null responsesRepresentation
                    then return Nothing
                    else do
                        let parts =
                                [ case splitOn ":" pair of
                                    [nm, val] ->
                                        case readMay val of
                                            Just i -> Just (Name (stringToText nm), i)
                                            Nothing -> Nothing
                                    _ -> Nothing
                                | pair <- splitOn "," responsesRepresentation
                                ]
                        let partsJust = catMaybes parts
                        if length parts == length partsJust
                            then return (Just partsJust)
                            else userErr1 $ vcat [ "Cannot parse the value for --responses-representation."
                                                 , "Expected a comma separated list of variable name : integer pairs."
                                                 , "But got:" <+> pretty responsesRepresentation
                                                 ]

            return Config.Config
                { Config.outputDirectory            = outputDirectory
                , Config.logLevel                   = logLevel
                , Config.verboseTrail               = verboseTrail
                , Config.rewritesTrail              = rewritesTrail
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
                , Config.representationLevels       = representationLevels
                , Config.limitModels                = if limitModels == Just 0 then Nothing else limitModels
                , Config.numberingStart             = numberingStart
                , Config.smartFilenames             = smartFilenames
                , Config.lineWidth                  = lineWidth
                , Config.responses                  = responsesList
                , Config.responsesRepresentation    = responsesRepresentationList
                , Config.estimateNumberOfModels     = estimateNumberOfModels
                }

    essenceM <- readModelFromFile essence
    liftIO $ hSetBuffering stdout LineBuffering
    liftIO $ maybe (return ()) setRandomSeed seed
    let
        conjuring = do
            config <- getConfig
            runNameGen essenceM $ outputModels portfolioSize modelHashesBefore modelNamePrefix config essenceM
    conjuring
mainWithArgs_Modelling _ _ _ _ = bug "mainWithArgs_Modelling"

pp :: MonadIO m => LogLevel -> Doc -> m ()
pp LogNone = const $ return ()
pp _       = liftIO . putStrLn . renderNormal


savilerowScriptName :: Sys.FilePath
savilerowScriptName
    | os `elem` ["darwin", "linux"] = "savilerow"
    | os `elem` ["mingw32"] = "savilerow.bat"
    | otherwise = bug "Cannot detect operating system."


quoteMultiWord :: String -> String
quoteMultiWord s
    | ' ' `elem` s = "\"" ++ s ++ "\""
    | otherwise = s


savileRowNoParam ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    UI ->
    (FilePath, Model) ->        -- model
    IO (Either
     [Doc]                      -- user error
     [ ( FilePath               -- model
       , FilePath               -- param
       , Maybe FilePath         -- solution, Nothing if solutionsInOneFile=True
       ) ])
savileRowNoParam ui@Solve{..} (modelPath, eprimeModel) = sh $ errExit False $ do
    pp logLevel $ hsep ["Savile Row:", pretty modelPath]
    let outBase = dropExtension modelPath
    srArgs <- liftIO $ srMkArgs ui outBase modelPath
    let tr = translateSolution eprimeModel def
    when (logLevel >= LogDebug) $ do
        liftIO $ putStrLn "Using the following options for Savile Row:"
        liftIO $ putStrLn $ "    savilerow " ++ unwords (map (quoteMultiWord . textToString) srArgs)
    (stdoutSR, solutions) <- partitionEithers <$> runHandle savilerowScriptName srArgs
                                (liftIO . srStdoutHandler
                                    (outBase, modelPath, "<no param file>", ui)
                                    tr (1::Int))
    srCleanUp (stringToText $ unlines stdoutSR) solutions
savileRowNoParam _ _ = bug "savileRowNoParam"


savileRowWithParams ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    UI ->
    (FilePath, Model) ->        -- model
    (FilePath, Model) ->        -- param
    IO (Either
     [Doc]                      -- user error
     [ ( FilePath               -- model
       , FilePath               -- param
       , Maybe FilePath         -- solution, Nothing if solutionsInOneFile=True
       ) ])
savileRowWithParams ui@Solve{..} (modelPath, eprimeModel) (paramPath, essenceParam) = sh $ errExit False $ do
    pp logLevel $ hsep ["Savile Row:", pretty modelPath, pretty paramPath]
    let outBase = dropExtension modelPath ++ "-" ++ dropDirs (dropExtension paramPath)
    let
        -- this is a bit tricky.
        -- we want to preserve user-erors, and not raise them as errors using IO.fail
        runTranslateParameter :: IO (Either [Doc] Model)
        runTranslateParameter = runUserErrorT $ ignoreLogs $ runNameGen () $
                                    translateParameter eprimeModel essenceParam
    eprimeParam' <- liftIO runTranslateParameter
    case eprimeParam' of
        Left err -> return (Left err)
        Right eprimeParam -> do
            liftIO $ writeFile (outputDirectory </> outBase ++ ".eprime-param") (render lineWidth eprimeParam)
            srArgsBase <- liftIO $ srMkArgs ui outBase modelPath
            let srArgs = "-in-param"
                       : stringToText (outputDirectory </> outBase ++ ".eprime-param")
                       : srArgsBase
            let tr = translateSolution eprimeModel essenceParam
            when (logLevel >= LogDebug) $ do
                liftIO $ putStrLn "Using the following options for Savile Row:"
                liftIO $ putStrLn $ "    savilerow " ++ unwords (map (quoteMultiWord . textToString) srArgs)
            (stdoutSR, solutions) <- partitionEithers <$> runHandle savilerowScriptName srArgs
                                        (liftIO . srStdoutHandler
                                            (outBase, modelPath, paramPath, ui)
                                            tr (1::Int))
            srCleanUp (stringToText $ unlines stdoutSR) solutions
savileRowWithParams _ _ _ = bug "savileRowWithParams"


srMkArgs :: UI -> FilePath -> FilePath -> IO [Text]
srMkArgs Solve{..} outBase modelPath = do
    let genericOpts =
            [ "-in-eprime"      , stringToText $ outputDirectory </> modelPath
            , "-out-minion"     , stringToText $ outputDirectory </> outBase ++ ".eprime-minion"
            , "-out-sat"        , stringToText $ outputDirectory </> outBase ++ ".eprime-dimacs"
            , "-out-aux"        , stringToText $ outputDirectory </> outBase ++ ".eprime-aux"
            , "-out-info"       , stringToText $ outputDirectory </> outBase ++ ".eprime-info"
            , "-out-minizinc"   , stringToText $ outputDirectory </> outBase ++ ".eprime.mzn"
            , "-run-solver"
            , "-S0"
            , "-solutions-to-stdout-one-line"
            ] ++
            [ "-cgroups" | cgroups ] ++
            ( if nbSolutions == "all"
                then ["-all-solutions"]
                else ["-num-solutions", stringToText nbSolutions]
            )

    solverSelection <- case solver of
        "minion"            -> return [ "-minion" ]
        "gecode"            -> return [ "-gecode" ]
        "chuffed"           -> return [ "-chuffed"]
        "glucose"           -> return [ "-sat"
                                      , "-sat-family", "glucose"
                                      , "-satsolver-bin", "glucose"
                                      ]
        "glucose-syrup"     -> return [ "-sat"
                                      , "-sat-family", "glucose"
                                      , "-satsolver-bin", "glucose-syrup"
                                      ]
        "cadical"           -> return [ "-sat"
                                      , "-sat-family", "lingeling"
                                      , "-satsolver-bin", "cadical"
                                      ]
        "lingeling"         -> return [ "-sat"
                                      , "-sat-family", "lingeling"
                                      , "-satsolver-bin", "lingeling"
                                      ]
        "plingeling"        -> return [ "-sat"
                                      , "-sat-family", "lingeling"
                                      , "-satsolver-bin", "plingeling"
                                      ]
        "treengeling"       -> return [ "-sat"
                                      , "-sat-family", "lingeling"
                                      , "-satsolver-bin", "treengeling"
                                      ]
        "minisat"           -> return [ "-sat"
                                      , "-sat-family", "minisat"
                                      , "-satsolver-bin", "minisat"
                                      ]
        "bc_minisat_all"    -> return [ "-sat"
                                      , "-sat-family", "bc_minisat_all"
                                      , "-satsolver-bin", "bc_minisat_all_release"
                                      ]
        "nbc_minisat_all"   -> return [ "-sat"
                                      , "-sat-family", "nbc_minisat_all"
                                      , "-satsolver-bin", "nbc_minisat_all_release"
                                      ]
        "open-wbo"          -> return [ "-maxsat"
                                      , "-satsolver-bin", "open-wbo"
                                      ]
        "coin-or"           -> return [ "-minizinc"
                                      , "-solver-options", "--solver COIN-BC"
                                      ]
        "cplex"             -> do
            env <- getEnvironment
            case lookup "CPLEX_PATH" env of
                Nothing -> userErr1 $ vcat
                    [ "Set environment variable CPLEX_PATH. Something like:"
                    , "    CPLEX_PATH=/path/to/cplex/library conjure solve"
                    ]
                Just cplex_path ->
                    return [ "-minizinc"
                           , "-solver-options", stringToText ("--solver CPLEX --cplex-dll " ++ cplex_path)
                           ]
        _ -> userErr1 ("Unknown solver:" <+> pretty solver)

    return $ genericOpts
          ++ solverSelection
          ++ map stringToText (concatMap words savilerowOptions)
          ++ if null solverOptions
                    then []
                    else [ "-solver-options"
                         , stringToText (unwords (concatMap words solverOptions))
                         ]
srMkArgs _ _ _ = bug "srMkArgs"


srStdoutHandler ::
    (FilePath, FilePath, FilePath, UI) ->
    (Model -> NameGenM (IdentityT IO) Model) ->
    Int ->
    Handle ->
    IO [Either String (FilePath, FilePath, Maybe FilePath)]
srStdoutHandler
        args@(outBase , modelPath, paramPath , Solve{..})
        tr solutionNumber h = do
    eof <- hIsEOF h
    if eof
        then do
            hClose h
            return []
        else do
            line <- hGetLine h
            case stripPrefix "Solution: " line of
                Nothing -> do
                    if isPrefixOf "Created output file for domain filtering" line
                        then pp logLevel $ hsep ["Running minion for domain filtering."]
                        else if isPrefixOf "Created output" line
                            then pp logLevel $ hsep ["Running solver:", pretty solver]
                            else return ()
                    fmap (Left line :)
                         (srStdoutHandler args tr solutionNumber h)
                Just solutionText -> do
                    eprimeSol  <- readModel ParserC.parseModel (Just id) ("<memory>", stringToText solutionText)
                    essenceSol <- ignoreLogs $ runNameGen () $ tr eprimeSol
                    case solutionsInOneFile of
                        False -> do
                            let mkFilename ext = outputDirectory </> outBase
                                                        ++ "-solution" ++ padLeft 6 '0' (show solutionNumber)
                                                        ++ ext
                            let filenameEprimeSol  = mkFilename ".eprime-solution"
                            let filenameEssenceSol = mkFilename ".solution"
                            writeModel lineWidth Plain (Just filenameEprimeSol) eprimeSol
                            writeModel lineWidth Plain (Just filenameEssenceSol) essenceSol
                            when (outputFormat == JSON) $
                                writeModel lineWidth JSON (Just (filenameEssenceSol ++ ".json")) essenceSol
                            fmap (Right (modelPath, paramPath, Just filenameEssenceSol) :)
                                 (srStdoutHandler args tr (solutionNumber+1) h)
                        True -> do
                            let mkFilename ext = outputDirectory </> outBase
                                                        ++ ext
                            let filenameEprimeSol  = mkFilename ".eprime-solutions"
                            let filenameEssenceSol = mkFilename ".solutions"
                            -- remove the solutions files before writing the first solution
                            when (solutionNumber == 1) $ do
                                removeFileIfExists filenameEprimeSol
                                removeFileIfExists filenameEssenceSol
                            appendFile filenameEprimeSol  ("$ Solution: " ++ padLeft 6 '0' (show solutionNumber))
                            appendFile filenameEprimeSol  ("\n" ++ render lineWidth eprimeSol  ++ "\n\n")
                            appendFile filenameEssenceSol ("$ Solution: " ++ padLeft 6 '0' (show solutionNumber))
                            appendFile filenameEssenceSol ("\n" ++ render lineWidth essenceSol ++ "\n\n")
                            fmap (Right (modelPath, paramPath, Nothing) :)
                                 (srStdoutHandler args tr (solutionNumber+1) h)
srStdoutHandler _ _ _ _ = bug "srStdoutHandler"


srCleanUp :: Text -> sols -> Sh (Either [Doc] sols)
srCleanUp stdoutSR solutions = do
    stderrSR   <- lastStderr
    exitCodeSR <- lastExitCode
    let combinedSR = T.unlines [stdoutSR, stderrSR]
    if  | T.isInfixOf "Savile Row timed out." combinedSR ->
            return (Left ["Savile Row timed out."])
        | T.isInfixOf "where false" combinedSR ->
            return (Left [vcat [ "Invalid instance, a where statement evaluated to false."
                               , "(It can be an implicit where statement added by Conjure.)"
                               ]])
        | or [ T.isInfixOf "Exception in thread" combinedSR
             , T.isInfixOf "ERROR" combinedSR
             , T.isInfixOf "Sub-process exited with error code" combinedSR
             ] ->
             return (Left [vcat [ "Savile Row stdout:"    <+> pretty stdoutSR
                                , "Savile Row stderr:"    <+> pretty stderrSR
                                , "Savile Row exit-code:" <+> pretty exitCodeSR
                                ]])
        | exitCodeSR == 0 -> return (Right solutions)
        | otherwise ->
            return (Left [vcat [ "Savile Row stdout:"    <+> pretty stdoutSR
                               , "Savile Row stderr:"    <+> pretty stderrSR
                               , "Savile Row exit-code:" <+> pretty exitCodeSR
                               ]])


validateSolutionNoParam ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    UI -> FilePath -> IO ()
validateSolutionNoParam Solve{..} solutionPath = do
    pp logLevel $ hsep ["Validating solution:", pretty solutionPath]
    essenceM <- readModelFromFile essence
    solution <- readParamOrSolutionFromFile solutionPath
    [essenceM2, solution2] <- ignoreLogs $ runNameGen () $ resolveNamesMulti [essenceM, solution]
    failToUserError $ ignoreLogs $ runNameGen () $ validateSolution essenceM2 def solution2
validateSolutionNoParam _ _ = bug "validateSolutionNoParam"


validateSolutionWithParams ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    UI -> FilePath -> FilePath -> IO ()
validateSolutionWithParams Solve{..} solutionPath paramPath = do
    pp logLevel $ hsep ["Validating solution:", pretty paramPath, pretty solutionPath]
    essenceM <- readModelFromFile essence
    param    <- readParamOrSolutionFromFile paramPath
    solution <- readParamOrSolutionFromFile solutionPath
    [essenceM2, param2, solution2] <- ignoreLogs $ runNameGen () $ resolveNamesMulti [essenceM, param, solution]
    failToUserError $ ignoreLogs $ runNameGen () $ validateSolution essenceM2 param2 solution2
validateSolutionWithParams _ _ _ = bug "validateSolutionWithParams"


doIfNotCached
    :: (MonadIO m, Hashable h)
    => h                        -- thing to hash
    -> FilePath                 -- saved hashes file
    -> m a                      -- the result from cache
    -> m a                      -- the action
    -> m a                      -- the results and writing the new hashes in the file
doIfNotCached (show . hash -> h) savedHashesFile getResult act = do
    savedHashes <- liftIO $ readFileIfExists savedHashesFile
    let skip = Just h == savedHashes                -- skip if h was the hash in the file
    if skip
        then getResult
        else do
            res <- act
            liftIO $ writeFile savedHashesFile h
            return res


autoParallel :: [IO a] -> IO [a]
autoParallel = if numCapabilities > 1 then parallel else sequence


autoParallel_ :: [IO ()] -> IO ()
autoParallel_ = if numCapabilities > 1 then parallel_ else sequence_

