module Main where

import System.Environment ( getArgs )

import TestUtils ( runTest )
import Test.Language.EssenceParsePrint as FromFile ( allTests )

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fp] -> runTest . FromFile.allTests =<< readFile fp
        _    -> error "you need to give a file path."
