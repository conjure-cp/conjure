module Main where


import Data.List ( isSuffixOf )
import System.Environment ( getArgs )

import TestUtils ( runTest )
import Test.Language.Unit.FromFile   as UnitFromFile   ( allTests )
import Test.Language.Unit.Random     as UnitRandom     ( allTests )
import Test.Language.OnFile.Spec     as OnFileSpec     ( allTests )
import Test.Language.OnFile.RuleRepr as OnFileRuleRepr ( allTests )
import Utils ( ppShow )


main :: IO ()
main = do
    let
        run :: [String] -> [String] -> String -> IO ()
        run specs reprs s = do

            let (a,n) = UnitFromFile.allTests s
            putStrLn $ " -- Running user supplied tests (" ++ show n ++ " testing statements parsed)."
            runTest a

            putStrLn " -- Running randomly generated test cases."
            runTest =<< UnitRandom.allTests

            putStrLn " -- Running OnFileSpec tests."
            runTest $ OnFileSpec.allTests specs

            putStrLn " -- Running ReprParse tests."
            runTest $ OnFileRuleRepr.allTests reprs

            putStrLn " -- Finished."

    args <- getArgs
    let unit  = filter (".unit"    `isSuffixOf`) args
    let specs = filter (".essence" `isSuffixOf`) args
    let reprs = filter (".repr"    `isSuffixOf`) args
    case unit of
        []   -> run specs reprs ""
        [fp] -> run specs reprs =<< readFile fp
        _    -> error $ "you need to give exactly one file path which contains the unit tests."
                     ++ "\ngiven arguments:"
                     ++ "\n" ++ ppShow args
