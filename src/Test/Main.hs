module Main where

import Control.Applicative
import System.Environment ( getArgs )
import Test.HUnit ( test )

import TestUtils ( runTest )
import Test.Language.EssenceParsePrintFromFile as FromFile ( allTests )
import Test.Language.EssenceParsePrintRandom   as Random   ( allTests )

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fp] -> do
            a <- FromFile.allTests <$> readFile fp
            b <- Random.allTests
            runTest $ test (a : b)
        _    -> error "you need to give a file path."
