module Language.E.Pipeline.Driver where

import Language.E

import System.Directory ( createDirectoryIfMissing )
-- import System.Mem ( performGC )


driverConjure
    :: ([RuleRepr] -> [RuleRefn] -> Spec -> [(Either Doc Spec, LogTree)])
    -> FilePath -> [RuleRepr] -> [RuleRefn] -> Spec -> IO ()
driverConjure conj baseFilename reprs refns spec = do
    let nats = map (padShow 4) [ (1 :: Int) .. ]
    let mouts = conj reprs refns spec
    createDirectoryIfMissing True baseFilename
    forM_ (zip nats mouts) $ \ (i, (mout, logs)) -> do
        let mkOutFilename ext = baseFilename ++ "/" ++ i ++ ext
        writeFile (mkOutFilename ".log") (renderPretty $ prettyLogs logs)
        case mout of
            Left  x -> writeFile (mkOutFilename ".err") (renderPretty x)
            Right x -> writeFile (mkOutFilename ".eprime") (renderPretty x)
        -- performGC
        -- putStrLn "preformGC"

padShow :: Show a => Int -> a -> String
padShow n i = let s = show i in replicate (n - length s) '0' ++ s

