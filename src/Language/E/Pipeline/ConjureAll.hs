{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.E.Pipeline.ConjureAll
    ( conjureAllPure
    , conjureAllDriver
    ) where

import Language.E
import Language.E.Pipeline.ConjureRepr
import Language.E.Pipeline.ConjureRefn
import Language.E.Pipeline.Groom ( groomSpec )

import qualified Data.DList as DList


data Phase
    = Repr
    | Refn
    | Groom


conjureAllPure
    :: [RuleRepr] -> [RuleRefn] -> Spec
    -> ([Either Doc Spec], DList.DList NamedLog)
conjureAllPure reprs refns = runWriter . go Repr
    where
        toError :: String -> (CompError, Maybe Spec) -> Either Doc Spec
        toError msg = Left . prettyErrors (pretty $ "Error in phase: " ++ msg) . return

        go  :: (Functor m, MonadWriter (DList.DList NamedLog) m)
            => Phase -> Spec
            -> m [Either Doc Spec]
        go Repr s = case runCompEIdentity $ conjureRepr False s reprs of
            ([]  , []  , logs) -> do
                tell logs
                go Groom s
            (outs, errs, logs) -> do
                tell logs
                let errorsOut = map (toError "Repr") errs
                specsOut <- concatMapM (go Refn) outs
                return $ errorsOut ++ specsOut
        go Refn s = case runCompEIdentity $ conjureRefn False s refns of
            ([]  , []  , logs) -> tell logs >> go Groom s
            (outs, errs, logs) -> do
                tell logs
                let errorsOut = map (toError "Refn") errs
                specsOut <- concatMapM (go Repr) outs
                return $ errorsOut ++ specsOut
        go Groom s = case runCompEIdentity $ groomSpec s of
            ([out], _   , logs) -> do tell logs ; return [Right out]
            (_    , errs, logs) -> do tell logs ; return $ map (toError "Groom") errs


conjureAllDriver :: FilePath -> [RuleRepr] -> [RuleRefn] -> Spec -> IO ()
conjureAllDriver baseFilename reprs refns spec = do
    start <- getCPUTime
    let padShow n i = let s = show i in replicate (n - length s) '0' ++ s
    let nats = map (padShow 4) [ (1 :: Int) .. ]
    let (mouts, logs) = conjureAllPure reprs refns spec
    createDirectoryIfMissing True baseFilename
    forM_ (zip nats mouts) $ \ (i, mout) -> case mout of
        Left  x -> writeFile (baseFilename ++ "/" ++ i ++ ".essence.err")
                             (renderPretty x)
        Right x -> writeFile (baseFilename ++ "/" ++ i ++ ".essence.out")
                             (renderPretty x)
    end   <- getCPUTime
    let
        diff :: Double
        diff = fromIntegral (end - start) / (10^12)

    let logs' = case buildLog "TotalTime" $ pretty diff of
                    Nothing -> logs
                    Just l  -> logs `DList.snoc` l
    writeFile (baseFilename ++ ".logs") $ renderPretty $ prettyLogs logs'


