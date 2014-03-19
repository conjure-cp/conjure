{-# LANGUAGE OverloadedStrings #-}

module Language.E.Pipeline.Driver where

import Bug
import Language.E

import Data.Time.Clock ( getCurrentTime )
import System.Directory
import System.Mem ( performGC )


driverConjureSingle
    :: Bool                 -- generate the *.logs file or not
    -> Bool                 -- generate *.errors file or not
    -> Maybe FilePath       -- the output filepath
    -> [(Either Doc Spec, LogTree)]
    -> IO ()

-- don't generate *.error files
driverConjureSingle _ False _ [(Left x, _ )] = bug $ pretty x

-- to stdout
driverConjureSingle _ _ Nothing [(Right x, _logs)] = putStrLn $ renderNormal x
driverConjureSingle _ _ Nothing [(Left  x, _logs)] = bug $ pretty x

-- to file
driverConjureSingle logsOut _ (Just pathOut) [(Right x, logs)] = do
    toFile  pathOut              (renderNormal x)
    when logsOut $ toFile (pathOut ++ ".logs" ) (renderWide logs)
driverConjureSingle logsOut True (Just pathOut) [(Left  x, logs)] = do
    toFile (pathOut ++ ".error") (renderNormal x)
    when logsOut $ toFile (pathOut ++ ".logs" ) (renderWide logs)

-- multiple outputs generated
driverConjureSingle _ _ _ _ = bug "Generates multiple outputs, must be a bug. Sorry."


driverConjure
    :: ([RuleRepr] -> [RuleRefn] -> Spec -> [(Either Doc Spec, LogTree)])
    -> FilePath -> [RuleRepr] -> [RuleRefn] -> Spec -> IO ()
driverConjure conj baseFilename reprs refns spec = do
    let outDirPath = baseFilename
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

    let nats = map (padShowInt 4) [ (1 :: Int) .. ]
    let mouts = conj reprs refns spec
    forM_ (zip nats mouts) $ \ (i, (mout, logs)) -> do
        let mkOutFilename ext = baseFilename ++ "/" ++ i ++ ext
        case mout of
            Left  x -> do
                toFile (mkOutFilename ".error"      ) (renderNormal x)
                toFile (mkOutFilename ".error.logs" ) (renderWide logs)
            Right x -> do
                toFile (mkOutFilename ".eprime"     ) (renderNormal x)
                toFile (mkOutFilename ".eprime.logs") (renderWide logs)
        performGC

toFile :: FilePath -> String -> IO ()
toFile fp con = do
    curr <- getCurrentTime
    putStrLn $ "[Conjure] Created file (at " ++ take 19 (show curr) ++ ") " ++ fp
    writeFile fp con

