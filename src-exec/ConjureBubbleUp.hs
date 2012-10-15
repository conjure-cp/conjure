{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment ( getArgs )

import Language.E
import Language.E.Pipeline.ReadIn
import Language.E.Pipeline.BubbleUp ( conjureBubbleUp )


main :: IO ()
main = do
    args <- getArgs

    specFilename <- case filter (".essence" `isSuffixOf`) args of
                        [t] -> return t
                        _   -> error "Only 1 *.essence file."

    specPair <- pairWithContents specFilename
    [spec]   <- runCompEIO (readSpec specPair)
    outSpecs <- runCompEIO (conjureBubbleUp spec)
    writeSpecs specFilename "bubble" outSpecs

