module Main where

import Conjure.Prelude
import Conjure.UI ( UI(..), ui )
import Conjure.UI.MainHelper ( mainWithArgs )
import Conjure.Language.Pretty ( pretty )

-- base
import System.CPUTime ( getCPUTime )
import System.Environment ( withArgs )
import System.Timeout ( timeout )
import Text.Printf ( printf )

-- cmdargs
import System.Console.CmdArgs ( cmdArgs )


main :: IO ()
main = do
    args <- getArgs
    -- this is for backward compatibility, so bilal's old scripts continue to work.
    -- "refine-param", if given as the first arg (i.e. it is the command)
    -- is rewritten to "translate-parameter"
    let args' = case args of
                    ("refine-param":rest) -> "translate-parameter" : rest
                    _ -> args
    input <- withArgs args' (cmdArgs ui)
    let workload = runLoggerPipeIO (logLevel input) $ do
            logDebug ("Command line options: " <+> pretty (show input))
            mainWithArgs input
    case limitTime input of
        Just sec | sec > 0 -> do
            putStrLn $ "Running with a time limit of " ++ show sec ++ " seconds."
            res <- timeout (sec * 1000000) workload
            case res of
                Nothing -> do
                    cputime <- getCPUTime
                    let
                        -- cputime is returned in pico-seconds. arbitrary precision integer.
                        -- divide by 10^9 first. use arbitrary precision integer arithmetic.
                        -- do the last 10^3 division via double to get 3 significant digits after the integer part.
                        cputimeInSeconds :: Double
                        cputimeInSeconds = fromInteger (cputime `div` 1000000000) / 1000
                    putStrLn $ printf "Timed out. Total CPU time used is %.3f seconds." cputimeInSeconds
                Just () -> return ()
        _ -> workload

