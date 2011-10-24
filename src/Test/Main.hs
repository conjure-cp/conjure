module Main where

import System.Environment ( getArgs )

import TestUtils ( runTest, test )
import Test.Language.EssenceParsePrint as FromFile ( allTests )

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fp] -> runTest . FromFile.allTests =<< readFile fp
        -- [fp] -> do
        --     c <- readFile fp
        --     let cs = replicate 100 (FromFile.allTests c)
        --     runTest $ test cs
        _    -> error "you need to give a file path."
