{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.E.Pipeline.ConjureAll
    ( conjureAllPure
    , driverConjureAll
    , driverConjureAllSilent
    ) where

import Language.E
import Language.E.Pipeline.ConjureRepr
import Language.E.Pipeline.ConjureRefn
import Language.E.Pipeline.Groom ( groomSpec )

import qualified Data.DList as DList
import System.Directory ( createDirectoryIfMissing )


data Phase = Repr | Refn | Groom



conjureAllPure
    :: StdGen -> [RuleRepr] -> [RuleRefn] -> Spec
    -> ([Either Doc Spec], DList.DList NamedLog)
conjureAllPure gen reprs refns = runWriter . flip evalStateT gen . go Repr
    where
        toError :: String -> (CompError, Maybe Spec) -> Either Doc Spec
        toError msg = Left . prettyErrors (pretty $ "Error in phase: " ++ msg) . return

        go  :: (Functor m, MonadWriter (DList.DList NamedLog) m, MonadState StdGen m)
            => Phase -> Spec
            -> m [Either Doc Spec]
        go Repr s = do
            g <- get
            case runCompEIdentity g $ conjureRepr False s reprs of
                ([]  , []  , (logs, g')) -> do
                    tell logs
                    put g'
                    go Groom s
                (outs, errs, (logs, g')) -> do
                    tell logs
                    put g'
                    let errorsOut = map (toError "Repr") errs
                    specsOut <- concatMapM (go Refn) outs
                    return $ errorsOut ++ specsOut
        go Refn s = do
            g <- get
            case runCompEIdentity g $ conjureRefn False s refns of
                ([]  , []  , (logs, g')) -> tell logs >> put g' >> go Groom s
                (outs, errs, (logs, g')) -> do
                    tell logs
                    put g'
                    let errorsOut = map (toError "Refn") errs
                    specsOut <- concatMapM (go Repr) outs
                    return $ errorsOut ++ specsOut
        go Groom s = do
            g <- get
            case runCompEIdentity g $ groomSpec s of
                ([out], _   , (logs, g')) -> do tell logs ; put g' ; return [Right out]
                (_    , errs, (logs, g')) -> do tell logs ; put g' ; return $ map (toError "Groom") errs

padShow :: Show a => Int -> a -> String
padShow n i = let s = show i in replicate (n - length s) '0' ++ s

driverConjureAll :: FilePath -> [RuleRepr] -> [RuleRefn] -> Spec -> IO ()
driverConjureAll baseFilename reprs refns spec = do
    gen <- getStdGen
    timeDiff <- fmap snd $ timedIO $ do
        let nats = map (padShow 4) [ (1 :: Int) .. ]
        let (mouts, logs) = conjureAllPure gen reprs refns spec
        let errors = lefts  mouts
        let outs   = rights mouts
        case errors of
            (e:_) -> error $ renderPretty e
            _     -> do
                createDirectoryIfMissing True baseFilename
                printLogs logs
                forM_ (zip nats outs) $ \ (i, out) ->
                    writeFile (baseFilename ++ "/" ++ i ++ ".essence")
                              (renderPretty out)
    case buildLog "TotalTime" $ pretty timeDiff of
        Nothing -> return ()
        Just l  -> putStrLn $ renderPretty (prettyLog l)


driverConjureAllSilent :: FilePath -> [RuleRepr] -> [RuleRefn] -> Spec -> IO ()
driverConjureAllSilent baseFilename reprs refns spec = do
    gen <- getStdGen
    timeDiff <- fmap snd $ timedIO $ do
        let nats = map (padShow 4) [ (1 :: Int) .. ]
        let (mouts, logs) = conjureAllPure gen reprs refns spec
        createDirectoryIfMissing True baseFilename
        writeFile (baseFilename ++ ".logs") $ renderPretty $ prettyLogs logs
        forM_ (zip nats mouts) $ \ (i, mout) -> case mout of
            Left  x -> writeFile (baseFilename ++ "/" ++ i ++ ".essence.err")
                                 (renderPretty x)
            Right x -> writeFile (baseFilename ++ "/" ++ i ++ ".essence.out")
                                 (renderPretty x)
    case buildLog "TotalTime" $ pretty timeDiff of
        Nothing -> return ()
        Just l  -> appendFile (baseFilename ++ ".logs") $ '\n' : renderPretty (prettyLog l)


