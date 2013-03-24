{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Paths_conjure_cp ( getBinDir )
import Data.Char ( toLower )
import System.Environment ( getArgs, getProgName )
import System.Directory
import System.FilePath ( (</>) )
import qualified Data.ByteString as ByteString

import Language.E
import Language.E.Pipeline.ReadIn
import Language.E.Pipeline.ConjureRepr
import Language.E.Pipeline.ConjureRefn
import Language.E.Pipeline.Groom


rulesdbLoc :: IO FilePath
rulesdbLoc = liftM (++ "/conjure.rulesdb") getBinDir

queueLoc :: IO FilePath
queueLoc = liftM (++ "/conjure.queue") getBinDir

data ConjureMode
    = MakeBinary
    | ViewBinary
    | MakeRulesDB
    | Start
    | PhaseRepr0
    | PhaseRepr
    | PhaseRefn
    | PathOfRulesDB
    | PathOfQueueFile
    deriving Show

toConjureMode :: String -> Maybe ConjureMode
toConjureMode  = helper . map toLower
    where
        helper "makebinary"      = Just MakeBinary
        helper "viewbinary"      = Just ViewBinary
        helper "makerulesdb"     = Just MakeRulesDB
        helper "start"           = Just Start
        helper "phaserepr0"      = Just PhaseRepr0
        helper "phaserepr"       = Just PhaseRepr
        helper "phaserefn"       = Just PhaseRefn
        helper "pathofrulesdb"   = Just PathOfRulesDB
        helper "pathofqueuefile" = Just PathOfQueueFile
        helper _ = Nothing

data ConjureFilePath
    = EssencePath     { filePath :: FilePath }
    | EssenceBinPath  { filePath :: FilePath }
    | RuleRefnPath    { filePath :: FilePath }
    | RuleReprPath    { filePath :: FilePath }
    | RulesDBPath     { filePath :: FilePath }
    deriving Show

toConjureFilePath :: String -> Maybe ConjureFilePath
toConjureFilePath x = listToMaybe [ f x
                                  | (ext, f) <- extensions
                                  , ext `isSuffixOf` x
                                  ]
    where
        extensions = [ ( ".essence"        , EssencePath    )
                     , ( ".essence.binary" , EssenceBinPath )
                     , ( ".rule"           , RuleRefnPath   )
                     , ( ".repr"           , RuleReprPath   )
                     , ( ".rulesdb"        , RulesDBPath    )
                     ]

type ConjureArg = Either ConjureMode ConjureFilePath

toConjureArg :: String -> Maybe ConjureArg
toConjureArg x =  Left  <$> toConjureMode x
              <|> Right <$> toConjureFilePath x

data ConjureArgs =
     ConjureArgs
        ConjureMode
        Bool              -- --1 or not
        [ConjureFilePath]
        (Maybe FilePath)  -- output directory
        (Maybe FilePath)  -- named pipe to write commands for next jobs
    deriving Show

toConjureArgs :: [String] -> ConjureArgs
toConjureArgs xs =
    let ys         = mapMaybe toConjureArg xs
        modes      = lefts ys
        filepaths  = rights ys
        outDirPath = listToMaybe [ p | ("--outDir", p) <- zip xs (tail xs) ]
        queuePath  = listToMaybe [ p | ("--queue" , p) <- zip xs (tail xs) ]
        genOne     = not $ null  [ p | ("--1"     , p) <- zip xs (tail xs) ]
    in  case modes of
            [mode] -> ConjureArgs mode  genOne filepaths outDirPath queuePath
            _      -> ConjureArgs Start genOne filepaths outDirPath queuePath

mkAbsolute :: ConjureArgs -> IO ConjureArgs
mkAbsolute (ConjureArgs mode genOne xs ys zs) =
    ConjureArgs mode genOne
        <$> forM xs mkArg
        <*> forM ys mkFP
        <*> forM zs mkFP
    where
        mkFP p@('/':_) = return p
        mkFP p = do
            curr <- getCurrentDirectory
            return (curr </> p)

        mkArg p = do
            let fp = filePath p
            fp' <- mkFP fp
            return p { filePath = fp' }

main :: IO ()
main = do
    args <- mkAbsolute =<< toConjureArgs <$> getArgs
    case args of
        ConjureArgs MakeBinary  _   paths _ _ -> mapM_ makeBinary  paths
        ConjureArgs ViewBinary  _   paths _ _ -> mapM_ viewBinary  paths
        ConjureArgs MakeRulesDB _   paths _ _ -> makeRulesDB paths
        ConjureArgs Start       one paths outDirPath queuePath -> mapM_ (start one outDirPath queuePath) paths
        ConjureArgs PhaseRepr0  one paths outDirPath queuePath -> do reportArgs args
                                                                     phaseRepr0 one outDirPath queuePath paths
        ConjureArgs PhaseRepr   one paths outDirPath queuePath -> do reportArgs args
                                                                     phaseRepr  one outDirPath queuePath paths
        ConjureArgs PhaseRefn   one paths outDirPath queuePath -> do reportArgs args
                                                                     phaseRefn  one outDirPath queuePath paths
        ConjureArgs PathOfRulesDB   _ _ _ _ -> putStrLn =<< rulesdbLoc
        ConjureArgs PathOfQueueFile _ _ _ _ -> putStrLn =<< queueLoc

reportArgs :: ConjureArgs -> IO ()
reportArgs (ConjureArgs mode one paths _ _) = do
    progName <- getProgName
    putStr $ unwords
           $ progName
           : show mode
           : (if one then "--1" else "")
           : map filePath paths

makeBinary :: ConjureFilePath -> IO ()
makeBinary arg = case arg of
    EssencePath path -> do
        pair <- pairWithContents path
        x    <- handleInIOSingle =<< runCompEIOSingle
                "Parsing problem specification"
                (readSpec pair)
        ByteString.writeFile (path ++ ".binary") (encode (x, def :: LogTree))
    RuleRefnPath path -> do
        pair <- pairWithContents path
        x    <- handleInIOSingle =<< runCompEIOSingle
                "Parsing rules"
                (readRuleRefn pair)
        ByteString.writeFile (path ++ ".binary") (encode x)
    RuleReprPath path -> do
        pair <- pairWithContents path
        x    <- handleInIOSingle =<< runCompEIOSingle
                "Parsing rules"
                (readRuleRepr pair)
        ByteString.writeFile (path ++ ".binary") (encode x)
    _ -> return ()

viewBinary:: ConjureFilePath -> IO ()
viewBinary arg = case arg of
    EssenceBinPath path -> do
        (x :: Spec, _ :: LogTree) <- decodeFromFile path
        printPretty x
    RulesDBPath path -> do
        (ruleReprs, ruleRefns) :: RulesDB <- decodeFromFile path
        printPretty $ vcat
            [          pretty path               <+> "is a valid Conjure rules database file"
            , nest 4 $ pretty (length ruleReprs) <+> "representation selection rules"
            , nest 4 $ pretty (length ruleRefns) <+> "expression refinement rules"
            ]
    _ -> return ()

makeRulesDB :: [ConjureFilePath] -> IO ()
makeRulesDB args = do
    outfile <- rulesdbLoc
    rulesdb <- liftM mconcat $ forM args $ \ arg -> case arg of
            RuleRefnPath path -> do
                pair <- pairWithContents path
                x    <- handleInIOSingle =<< runCompEIOSingle
                        "Parsing rules"
                        (readRuleRefn pair)
                return ([],x)
            RuleReprPath path -> do
                pair <- pairWithContents path
                x    <- handleInIOSingle =<< runCompEIOSingle
                        "Parsing rules"
                        (readRuleRepr pair)
                return ([x],[])
            _ -> return ([],[])
    ByteString.writeFile outfile (encode (rulesdb :: RulesDB))

-- this is the entry point of conjure.
-- input is a single problem specification, an essence file
-- next phase is phaseRepr0
start :: Bool -> Maybe FilePath -> Maybe FilePath -> ConjureFilePath -> IO ()
start one moutDirPath mqueuePath (EssencePath path) = do

    let outDirPath = fromMaybe (dropExts path) moutDirPath
    b <- doesDirectoryExist outDirPath
    when b $ do
        let exts = [ ".eprime", ".eprime.logs"
                   , ".error" , ".error.logs"
                   , ".essence.binary"
                   ]
        cons <- getDirectoryContents outDirPath
        forM_ cons $ \ con ->
            when (any (`isSuffixOf` con) exts)
                 (removeFile $ outDirPath ++ "/" ++ con)
    createDirectoryIfMissing True outDirPath

    queuePath <- maybe queueLoc return mqueuePath

    essenceInp
        :: Spec
        <- do con <- pairWithContents path
              handleInIOSingle =<< runCompEIOSingle
                "Parsing problem specification"
                (readSpec con)

    essenceBinFileOut outDirPath essenceInp def
        queuePath
        (nextPhaseCmd "phaseRepr0" one outDirPath queuePath)
start _ _ _ _ = error "Provide *.essence files only"

-- special case for the very first repr phase
-- runs the repr phase once.
--  if ErrGeneratesNone --> runs refn once and outputs a *.eprime file
--  else                --> outputs each (Spec,LogFile) to a binary file
-- next phase is phaseRefn
phaseRepr0 :: Bool -> Maybe FilePath -> Maybe FilePath -> [ConjureFilePath] -> IO ()
phaseRepr0 one (Just outDirPath) (Just queuePath) [EssenceBinPath path] = do
    let takeOne = if one then take 1 else id

    (essenceInp :: Spec, logsInp :: LogTree) <- decodeFromFile path

    (ruleReprs, ruleRefns) :: RulesDB <- decodeFromFile =<< rulesdbLoc

    let
        results1 :: [(Either ConjureError Spec, LogTree)]
        results1 = takeOne $ runComp logsInp $ conjureRepr ruleReprs essenceInp

        final1 :: Bool
        final1 = not $ null [ () | (Left (ErrGeneratesNone, _, _), _) <- results1 ]

    if final1

        then do -- run refn and groom and output an *.eprime file
            let
                results2 :: [(Either ConjureError Spec, LogTree)]
                results2 = takeOne $ runComp logsInp $ conjureRefn ruleReprs ruleRefns essenceInp >>= groomSpec

            forM_ results2 $ \ result -> case result of
                (Left  x, logs) -> errorFileOut   outDirPath x logs
                (Right x, logs) -> essenceFileOut outDirPath x logs
            printPretty $ nest 4 ("{HALT " <> pretty (length results2) <> "}")

        else do -- create the binary file, and output the command to run next to stdout
            forM_ results1 $ \ result -> case result of
                (Left  x, logs) -> errorFileOut      outDirPath x logs
                (Right x, logs) -> essenceBinFileOut outDirPath x logs
                                    queuePath
                                    (nextPhaseCmd "phaseRefn" one outDirPath queuePath)
            printPretty ("\t{" <> pretty (length results1) <> "}")
    removeFile path
phaseRepr0 _ _ _ _ = error "Argument error in phaseRepr0"

-- runs the repr phase once.
--  if ErrGeneratesNone --> outputs a *.eprime file
--  else                --> outputs each (Spec,LogFile) to a binary file
-- next phase is phaseRefn
phaseRepr :: Bool -> Maybe FilePath -> Maybe FilePath -> [ConjureFilePath] -> IO ()
phaseRepr one (Just outDirPath) (Just queuePath) [EssenceBinPath path] = do
    let takeOne = if one then take 1 else id

    (essenceInp :: Spec, logsInp :: LogTree) <- decodeFromFile path

    (ruleReprs, _) :: RulesDB <- decodeFromFile =<< rulesdbLoc

    let
        results1 :: [(Either ConjureError Spec, LogTree)]
        results1 = takeOne $ runComp logsInp $ conjureRepr ruleReprs essenceInp

        final1 :: Bool
        final1 = not $ null [ () | (Left (ErrGeneratesNone, _, _), _) <- results1 ]

    if final1

        then do -- run groom and output an *.eprime file
            let
                result2 :: (Either ConjureError Spec, LogTree)
                result2 = runComp1 logsInp $ groomSpec essenceInp
            case result2 of
                (Left  x2, logs2) -> errorFileOut   outDirPath x2 logs2
                (Right x2, logs2) -> essenceFileOut outDirPath x2 logs2

            printPretty ("\t{" <> "HALT" <> "}")
        else do -- create the binary file, and output the command to run next to stdout
            forM_ results1 $ \ result -> case result of
                (Left  x, logs) -> errorFileOut      outDirPath x logs
                (Right x, logs) -> essenceBinFileOut outDirPath x logs
                                    queuePath
                                    (nextPhaseCmd "phaseRefn" one outDirPath queuePath)
            printPretty ("\t{" <> pretty (length results1) <> "}")
    removeFile path
phaseRepr _ _ _ _ = error "Argument error in phaseRepr"

-- runs the refn phase once.
-- next phase is phaseRepr
phaseRefn :: Bool -> Maybe FilePath -> Maybe FilePath -> [ConjureFilePath] -> IO ()
phaseRefn one (Just outDirPath) (Just queuePath) [EssenceBinPath path] = do
    let takeOne = if one then take 1 else id

    (essenceInp :: Spec, logsInp :: LogTree) <- decodeFromFile path

    (ruleReprs, ruleRefns) :: RulesDB <- decodeFromFile =<< rulesdbLoc

    let
        results1 :: [(Either ConjureError Spec, LogTree)]
        results1 = takeOne $ runComp logsInp $ conjureRefn ruleReprs ruleRefns essenceInp

    -- create the binary file, and output the command to run next to stdout
    forM_ results1 $ \ result -> case result of
        (Left  x, logs) -> errorFileOut      outDirPath x logs
        (Right x, logs) -> essenceBinFileOut outDirPath x logs
                                    queuePath
                                    (nextPhaseCmd "phaseRepr" one outDirPath queuePath)
    printPretty ("\t{" <> pretty (length results1) <> "}")
    removeFile path
phaseRefn _ _ _ _ = error "Argument error in phaseRefn"

nextPhaseCmd :: String -> Bool -> FilePath -> FilePath -> IO String
nextPhaseCmd ph one outDirPath queuePath = do
    progName <- getProgName
    return $ unwords
        [ progName
        , ph
        , if one then "--1" else ""
        , "--queue" , queuePath
        , "--outDir", outDirPath
        ]

dropExts :: FilePath -> FilePath
dropExts x =
    let ys = splitOn "/" x
        zs = splitOn "." (last ys)
    in  intercalate "/" $
            (if length ys > 1 then init ys else [])
            ++ [headNote "SmallStepMain.dropExts" zs]

nextFilePathWithExt :: FilePath -> String -> IO FilePath
nextFilePathWithExt base ext = do
    essenceBins
        :: [Int]
        <- liftM (mapMaybe maybeRead . filter (ext `isSuffixOf`))
                 (getDirectoryContents base)
    let nextInt = 1 + foldr max 0 essenceBins
    return (base ++ "/" ++ padShowInt 4 nextInt ++ ext)

errorFileOut :: FilePath -> ConjureError -> LogTree -> IO ()
errorFileOut base x logs = do
    path <- nextFilePathWithExt base ".error"
    writeFile path              (renderPretty x)
    writeFile (path ++ ".logs") (renderPretty logs)

essenceFileOut :: FilePath -> Spec -> LogTree -> IO ()
essenceFileOut base x logs = do
    path <- nextFilePathWithExt base ".eprime"
    writeFile path              (renderPretty x)
    writeFile (path ++ ".logs") (renderPretty logs)

essenceBinFileOut :: FilePath -> Spec -> LogTree -> FilePath -> IO String -> IO ()
essenceBinFileOut base x logs queuePath mCommand = do
    command <- mCommand
    let path = base ++ "/." ++ show (hash x) ++ ".essence.binary"
    ByteString.writeFile path (encode (x, logs))
    appendFile queuePath $ unwords [command, path] ++ "\n"

runComp
    :: LogTree
    -> FunkyMulti GlobalState ConjureState ConjureError Identity a
    -> [(Either ConjureError a, LogTree)]
runComp logs
    = map (second localLogs)
    . fst
    . runIdentity
    . runFunkyMulti def (def { localLogs = logs })

runComp1
    :: LogTree
    -> FunkySingle ConjureState ConjureError Identity a
    -> (Either ConjureError a, LogTree)
runComp1 logs
    = second localLogs
    . runIdentity
    . runFunkySingle (def { localLogs = logs })

