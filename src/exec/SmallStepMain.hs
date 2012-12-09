{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Paths_conjure_cp ( getBinDir )
import Data.Char ( toLower )
import System.Environment ( getArgs )
import System.Directory ( createDirectoryIfMissing, getDirectoryContents, removeFile )
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
    in  case modes of
            [mode] -> ConjureArgs mode  filepaths outDirPath queuePath
            _      -> ConjureArgs Start filepaths outDirPath queuePath

main :: IO ()
main = do
    args <- toConjureArgs <$> getArgs
    case args of
        ConjureArgs MakeBinary  paths _ _ -> mapM_ makeBinary  paths
        ConjureArgs ViewBinary  paths _ _ -> mapM_ viewBinary  paths
        ConjureArgs MakeRulesDB paths _ _ -> makeRulesDB paths
        ConjureArgs Start       paths outDirPath queuePath -> mapM_ (start outDirPath queuePath) paths
        ConjureArgs PhaseRepr0  paths outDirPath queuePath -> do reportArgs args
                                                                 phaseRepr0 outDirPath queuePath paths
        ConjureArgs PhaseRepr   paths outDirPath queuePath -> do reportArgs args
                                                                 phaseRepr  outDirPath queuePath paths
        ConjureArgs PhaseRefn   paths outDirPath queuePath -> do reportArgs args
                                                                 phaseRefn  outDirPath queuePath paths
        ConjureArgs PathOfRulesDB   _ _ _ -> putStrLn =<< rulesdbLoc
        ConjureArgs PathOfQueueFile _ _ _ -> putStrLn =<< queueLoc

reportArgs :: ConjureArgs -> IO ()
reportArgs (ConjureArgs mode paths _ _)
    = putStrLn
    $ unwords
    $ "conjure" : show mode : map filePath paths

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
start :: Maybe FilePath -> Maybe FilePath -> ConjureFilePath -> IO ()
start moutDirPath mqueuePath (EssencePath path) = do

    let outDirPath = fromMaybe (dropExts path) moutDirPath
    queuePath <- maybe queueLoc return mqueuePath

    essenceInp
        :: Spec
        <- do con <- pairWithContents path
              handleInIOSingle =<< runCompEIOSingle
                "Parsing problem specification"
                (readSpec con)

    essenceBinFileOut outDirPath essenceInp def
        queuePath
        (nextPhaseCmd "phaseRepr0" outDirPath queuePath)
start _ _ _ = error "Provide a *.essence files only"

-- special case for the very first repr phase
-- runs the repr phase once.
--  if ErrGeneratesNone --> runs refn once and outputs a *.eprime file
--  else                --> outputs each (Spec,LogFile) to a binary file
-- next phase is phaseRefn
phaseRepr0 :: Maybe FilePath -> Maybe FilePath -> [ConjureFilePath] -> IO ()
phaseRepr0 (Just outDirPath) (Just queuePath) [EssenceBinPath path] = do

    (essenceInp :: Spec, logsInp :: LogTree) <- decodeFromFile path

    (ruleReprs, ruleRefns) :: RulesDB <- decodeFromFile =<< rulesdbLoc

    let
        results1 :: [(Either ConjureError Spec, LogTree)]
        results1 = runComp logsInp $ conjureRepr ruleReprs essenceInp

        final1 :: Bool
        final1 = not $ null [ () | (Left (ErrGeneratesNone, _, _), _) <- results1 ]

    if final1

        then do -- run refn and groom and output an *.eprime file
            let
                results2 :: [(Either ConjureError Spec, LogTree)]
                results2 = runComp logsInp $ conjureRefn ruleRefns essenceInp >>= groomSpec

            forM_ results2 $ \ result -> case result of
                (Left  x, logs) -> errorFileOut   outDirPath x logs
                (Right x, logs) -> essenceFileOut outDirPath x logs

        else -- create the binary file, and output the command to run next to stdout
            forM_ results1 $ \ result -> case result of
                (Left  x, logs) -> errorFileOut      outDirPath x logs
                (Right x, logs) -> essenceBinFileOut outDirPath x logs
                                    queuePath
                                    (nextPhaseCmd "phaseRefn" outDirPath queuePath)
    removeFile path
phaseRepr0 _ _ _ = error "Argument error in phaseRepr0"

-- runs the repr phase once.
--  if ErrGeneratesNone --> outputs a *.eprime file
--  else                --> outputs each (Spec,LogFile) to a binary file
-- next phase is phaseRefn
phaseRepr :: Maybe FilePath -> Maybe FilePath -> [ConjureFilePath] -> IO ()
phaseRepr (Just outDirPath) (Just queuePath) [EssenceBinPath path] = do

    (essenceInp :: Spec, logsInp :: LogTree) <- decodeFromFile path

    (ruleReprs, _) :: RulesDB <- decodeFromFile =<< rulesdbLoc

    let
        results1 :: [(Either ConjureError Spec, LogTree)]
        results1 = runComp logsInp $ conjureRepr ruleReprs essenceInp

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

        else -- create the binary file, and output the command to run next to stdout
            forM_ results1 $ \ result -> case result of
                (Left  x, logs) -> errorFileOut      outDirPath x logs
                (Right x, logs) -> essenceBinFileOut outDirPath x logs
                                    queuePath
                                    (nextPhaseCmd "phaseRefn" outDirPath queuePath)
    removeFile path
phaseRepr _ _ _ = error "Argument error in phaseRepr"

-- runs the refn phase once.
-- next phase is phaseRepr
phaseRefn :: Maybe FilePath -> Maybe FilePath -> [ConjureFilePath] -> IO ()
phaseRefn (Just outDirPath) (Just queuePath) [EssenceBinPath path] = do

    (essenceInp :: Spec, logsInp :: LogTree) <- decodeFromFile path

    (_, ruleRefns) :: RulesDB <- decodeFromFile =<< rulesdbLoc

    let
        results1 :: [(Either ConjureError Spec, LogTree)]
        results1 = runComp logsInp $ conjureRefn ruleRefns essenceInp

    -- create the binary file, and output the command to run next to stdout
    forM_ results1 $ \ result -> case result of
        (Left  x, logs) -> errorFileOut      outDirPath x logs
        (Right x, logs) -> essenceBinFileOut outDirPath x logs
                                    queuePath
                                    (nextPhaseCmd "phaseRepr" outDirPath queuePath)
    removeFile path
phaseRefn _ _ _ = error "Argument error in phaseRefn"

nextPhaseCmd :: String -> FilePath -> FilePath -> String
nextPhaseCmd ph outDirPath queuePath
    = unwords [ "conjure"
              , ph
              , "--outDir", outDirPath
              , "--queue" , queuePath
              ]

dropExts :: FilePath -> FilePath
dropExts x =
    let ys = splitOn "/" x
        zs = splitOn "." (last ys)
    in  intercalate "/" $
            (if length ys > 1 then init ys else [])
            ++ [head zs]

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
    createDirectoryIfMissing True base
    path <- nextFilePathWithExt base ".error"
    writeFile path              (renderPretty x)
    writeFile (path ++ ".logs") (renderPretty logs)

essenceFileOut :: FilePath -> Spec -> LogTree -> IO ()
essenceFileOut base x logs = do
    createDirectoryIfMissing True base
    path <- nextFilePathWithExt base ".essence"
    writeFile path              (renderPretty x)
    writeFile (path ++ ".logs") (renderPretty logs)

essenceBinFileOut :: FilePath -> Spec -> LogTree -> FilePath -> String -> IO ()
essenceBinFileOut base x logs queuePath pre = do
    createDirectoryIfMissing True base
    path <- nextFilePathWithExt base ".essence.binary"
    ByteString.writeFile path (encode (x, logs))
    appendFile queuePath $ unwords [pre, path] ++ "\n"

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

