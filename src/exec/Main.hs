module Main where

import Conjure.Prelude
import Conjure.UI ( UI(..), ui )
import Conjure.UI.MainHelper ( mainWithArgs )
import Conjure.Language.Pretty ( pretty )
import Conjure.Language.Type ( TypeCheckerMode(..) )
import Conjure.UserError ( userErr1 )

-- base
import System.CPUTime ( getCPUTime )
import System.Environment ( withArgs )
import System.Timeout ( timeout )
import Text.Printf ( printf )
import Data.Char ( isDigit )

-- cmdargs
import System.Console.CmdArgs ( cmdArgs )

-- terminal-size
import qualified System.Console.Terminal.Size as Terminal ( size, width )

-- pretty-show
import Text.Show.Pretty ( ppDoc )


main :: IO ()
main = do
    let
        -- this is for backward compatibility, so bilal's old scripts continue to work.
        -- "refine-param", if given as the first arg (i.e. it is the command)
        -- is rewritten to "translate-parameter"
        compatRefineParam args =
            case args of
                ("refine-param":rest) -> "translate-parameter" : rest
                _ -> args

        -- if no arguments are given, print the help message
        noArgPrintsHelp args =
            if null args
                then ["--help"]
                else args

        -- if a width is not specified, we try to find out their terminal width automatically
        -- and use the full width.
        helpAutoWidth args = do
            let
                rawHelpVals = mapMaybe (stripPrefix "--help=") args
                helpVals = concatMap (splitOn ",") rawHelpVals
                containsWidth = any (all isDigit) helpVals
                containsHelp = any (isPrefixOf "--help") args
            case (containsHelp, containsWidth) of
                (False, _) -> return args                           -- no "--help*"s were given
                (_, True) -> return args                            -- a width was specified
                _ -> do                                             -- use the full with of the terminal
                    terminalSize <- Terminal.size
                    case terminalSize of
                        Nothing -> return args                      -- cannot work out the terminal size
                        Just s  -> do
                            let terminalWidth = Terminal.width s
                            return $ args ++ ["--help=" ++ intercalate "," (helpVals ++ [show (terminalWidth :: Int)])]

    args  <- getArgs >>= return . compatRefineParam
                     >>= return . noArgPrintsHelp
                     >>= helpAutoWidth
    input <- withArgs args (cmdArgs ui)
    let workload = runLoggerPipeIO (logLevel input) $ do
            when (estimateNumberOfModels input) $
                liftIO $ print $ vcat ["Command line options", ppDoc input]
            logDebug ("Command line options:" <+> pretty (show input))
            let ?typeCheckerMode = StronglyTyped
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

                        msg :: String
                        msg = printf "Timed out. Total CPU time used by Conjure is %.3f seconds." cputimeInSeconds

                    userErr1 (pretty msg)
                Just () -> return ()
        _ -> workload

