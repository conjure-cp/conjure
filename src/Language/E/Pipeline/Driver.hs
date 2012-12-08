module Language.E.Pipeline.Driver where

import Language.E

import System.Directory ( createDirectoryIfMissing )
-- import System.Mem ( performGC )


driverConjure
    :: ([RuleRepr] -> [RuleRefn] -> Spec -> [(Either Doc Spec, LogTree)])
    -> FilePath -> [RuleRepr] -> [RuleRefn] -> Spec -> IO ()
driverConjure conj baseFilename reprs refns spec = do
    let nats = map (padShowInt 4) [ (1 :: Int) .. ]
    let mouts = conj reprs refns spec
    createDirectoryIfMissing True baseFilename
    forM_ (zip nats mouts) $ \ (i, (mout, logs)) -> do
        let mkOutFilename ext = baseFilename ++ "/" ++ i ++ ext
        toFile (mkOutFilename ".log") logs
        case mout of
            Left  x -> toFile (mkOutFilename ".err"   ) x
            Right x -> toFile (mkOutFilename ".eprime") x
        -- performGC
        -- putStrLn "preformGC"

toFile :: Pretty p => FilePath -> p -> IO ()
toFile fp con = do
    -- putStrLn $ "Created: " ++ fp
    writeFile fp (renderPretty con)

