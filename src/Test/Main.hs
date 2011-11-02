module Main where


import Data.List ( isSuffixOf )
import System.Environment ( getArgs )

import TestUtils ( runTest )
import Test.Language.EssenceParsePrintFromFile as FromFile ( allTests )
import Test.Language.EssenceParsePrintRandom   as Random   ( allTests )
import Test.Language.SpecParse as SpecParse ( allTests )
import Utils ( ppShow )


main :: IO ()
main = do
    let
        run :: [String] -> String -> IO ()
        run specs s = do

            let (a,n) = FromFile.allTests s
            putStrLn $ " -- Running user supplied tests (" ++ show n ++ " testing statements parsed)."
            runTest a

            b <- Random.allTests
            putStrLn " -- Running randomly generated test cases."
            runTest b

            c <- SpecParse.allTests specs
            putStrLn " -- Running SpecParse tests."
            runTest c

            putStrLn " -- Finished."

    args <- getArgs
    let specs = filter (".essence" `isSuffixOf`) args
    let unit  = filter (".unit" `isSuffixOf`) args
    case unit of
        [fp] -> run specs =<< readFile fp
        _    -> error $ "you need to give exactly one file path which contains the unit tests."
                     ++ "\ngiven arguments:"
                     ++ "\n" ++ ppShow args
