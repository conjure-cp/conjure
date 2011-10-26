module Main where

import System.Environment ( getArgs )
import Test.HUnit ( test )

import TestUtils ( runTest )
import Test.Language.EssenceParsePrintFromFile as FromFile ( allTests )
import Test.Language.EssenceParsePrintRandom   as Random   ( allTests )

main :: IO ()
main = do
    let
        run :: String -> IO ()
        run s = do
            let (a,n) = FromFile.allTests s
            putStrLn $ "Running user supplied tests (" ++ show n ++ ")"
            runTest a
            b <- Random.allTests
            putStrLn "Running randomly generated test cases"
            runTest (test b)
            putStrLn "Finished"

    args <- getArgs
    case args of
        []   -> run =<< getContents
        [fp] -> run =<< readFile fp
        _    -> error "you need to give a file path."
