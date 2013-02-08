module Language.E.Pipeline.Driver where

import Language.E

import System.Directory
-- import System.Mem ( performGC )


driverConjure
    :: ([RuleRepr] -> [RuleRefn] -> Spec -> [(Either Doc Spec, LogTree)])
    -> FilePath -> [RuleRepr] -> [RuleRefn] -> Spec -> IO ()
driverConjure conj baseFilename reprs refns spec = do
    let nats = map (padShowInt 4) [ (1 :: Int) .. ]
    let mouts = conj reprs refns spec

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

    forM_ (zip nats mouts) $ \ (i, (mout, logs)) -> do
        let mkOutFilename ext = baseFilename ++ "/" ++ i ++ ext
        case mout of
            Left  x -> do
                toFile (mkOutFilename ".error"      ) x
                toFile (mkOutFilename ".error.logs" ) logs
            Right x -> do
                toFile (mkOutFilename ".eprime"     ) x
                toFile (mkOutFilename ".eprime.logs") logs
        -- performGC
        -- putStrLn "preformGC"

toFile :: Pretty p => FilePath -> p -> IO ()
toFile fp con = do
    -- putStrLn $ "Created: " ++ fp
    writeFile fp (renderPretty con)

