{-# LANGUAGE OverloadedStrings #-}

module Language.E.Pipeline.Driver where

import Language.E

import Data.Time.Clock ( getCurrentTime )
import System.Directory
import System.Mem ( performGC )


driverConjureSingle
    :: Bool     -- generate the *.logs file or not
    -> FilePath -- the output filepath
    -> [(Either Doc Spec, LogTree)]
    -> IO ()
driverConjureSingle logsOut pathOut [(Right x, logs)] = do
    toFile  pathOut              x
    when logsOut $ toFile (pathOut ++ ".logs" ) logs
driverConjureSingle logsOut pathOut [(Left  x, logs)] = do
    toFile (pathOut ++ ".error") x
    when logsOut $ toFile (pathOut ++ ".logs" ) logs
driverConjureSingle _ _ _ = bug "Generates multiple outputs, must be a bug. Sorry."


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
                toFile (mkOutFilename ".error"      ) x
                toFile (mkOutFilename ".error.logs" ) logs
            Right x -> do
                toFile (mkOutFilename ".eprime"     ) x
                toFile (mkOutFilename ".eprime.logs") logs
        performGC

toFile :: Pretty p => FilePath -> p -> IO ()
toFile fp con = do
    curr <- getCurrentTime
    putStrLn $ "[Conjure] Created file (at " ++ take 19 (show curr) ++ ") " ++ fp
    writeFile fp (renderPretty con)

