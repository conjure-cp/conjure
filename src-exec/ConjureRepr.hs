{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment ( getArgs )

import Language.E
import Language.E.Pipeline.ReadIn
import Language.E.Pipeline.ConjureRepr ( conjureRepr )


main :: IO ()
main = do
    args <- getArgs

    specFilename <- case filter (".essence" `isSuffixOf`) args of
                        [t] -> return t
                        _   -> error "Only 1 *.essence file."

    let reprFilenames = filter (".repr" `isSuffixOf`) args
    when (null reprFilenames)
        $ putStrLn "Warning: no *.repr file is given."

    specPair  <- pairWithContents specFilename
    reprPairs <- mapM pairWithContents reprFilenames

    [spec ] <- runCompEIO (readSpec specPair)
    [reprs] <- runCompEIO (mapM readRuleRepr reprPairs)

    outSpecs <- runCompEIO (conjureRepr True spec reprs)

    writeSpecs specFilename "repr" outSpecs

