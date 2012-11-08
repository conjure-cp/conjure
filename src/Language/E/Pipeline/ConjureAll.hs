{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.E.Pipeline.ConjureAll
    ( conjureAllPure
    , driverConjureAll
    ) where

import Language.E
import Language.E.Pipeline.ConjureRepr
import Language.E.Pipeline.ConjureRefn
import Language.E.Pipeline.Groom ( groomSpec )
import Language.E.Pipeline.InlineLettings

import qualified Data.DList as DList
import System.Directory ( createDirectoryIfMissing )


data Phase = Repr | Refn | Groom



conjureAllPure
    :: StdGen -> [RuleRepr] -> [RuleRefn] -> Spec
    -> [(Either Doc Spec, DList.DList NamedLog)]
conjureAllPure gen reprs refns
    = onlyOneError
    . runIdentity . flip evalStateT gen . go Repr
    where
        onlyOneError [] = []
        onlyOneError (x:xs)
            | isLeft (fst x) = [x]
            | otherwise      = x : onlyOneError xs

        toError :: String -> (CompError, Maybe Spec) -> Either Doc Spec
        toError msg = Left . prettyErrors (pretty $ "Error in phase: " ++ msg) . return

        go  :: (Functor m, MonadState StdGen m)
            => Phase -> Spec
            -> m [(Either Doc Spec, DList.DList NamedLog)]
        go Repr s = do
            g <- get
            let (mouts, (_, g')) = runIdentity $ runCompE g $ inlineLettings s >>= \ s' -> conjureRepr False s' reprs
            put g'
            if null mouts
                then go Groom s
                else flip concatMapM mouts $ \ (mout, locSt) -> case mout of
                        Left  x -> return [(toError "Repr" x, localLogs locSt)]
                        Right x -> map (second (localLogs locSt `DList.append`)) <$> go Refn x
        go Refn s = do
            g <- get
            let (mouts, (_, g')) = runIdentity $ runCompE g $ conjureRefn False s refns
            put g'
            if null mouts
                then go Groom s
                else flip concatMapM mouts $ \ (mout, locSt) -> case mout of
                        Left  x -> return [(toError "Refn" x, localLogs locSt)]
                        Right x -> map (second (localLogs locSt `DList.append`)) <$> go Repr x
        go Groom s = do
            g <- get
            let (mouts, _) = runIdentity $ runCompE g $ groomSpec s
            return $ flip map mouts $ \ (mout, locSt) -> case mout of
                Left  x -> (toError "Groom" x, localLogs locSt)
                Right x -> (Right x          , localLogs locSt)

padShow :: Show a => Int -> a -> String
padShow n i = let s = show i in replicate (n - length s) '0' ++ s

driverConjureAll :: FilePath -> [RuleRepr] -> [RuleRefn] -> Spec -> IO ()
driverConjureAll baseFilename reprs refns spec = do
    gen <- getStdGen
    let nats = map (padShow 4) [ (1 :: Int) .. ]
    let mouts = conjureAllPure gen reprs refns spec
    createDirectoryIfMissing True baseFilename
    forM_ (zip nats mouts) $ \ (i, (mout, logs)) -> do
        let mkOutFilename ext = baseFilename ++ "/" ++ i ++ ext
        writeFile (mkOutFilename ".log") (renderPretty $ prettyLogs logs)
        case mout of
            Left  x -> writeFile (mkOutFilename ".err") (renderPretty x)
            Right x -> writeFile (mkOutFilename ".eprime") (renderPretty x)


