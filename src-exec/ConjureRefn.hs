{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment ( getArgs )

import Language.E
import Language.E.Pipeline.ReadIn
import Language.E.Pipeline.ConjureRefn ( conjureRefn )


main :: IO ()
main = do
    args <- getArgs

    specFilename <- case filter (".essence" `isSuffixOf`) args of
                        [t] -> return t
                        _   -> error "Only 1 *.essence file."

    let refnFilenames = filter (".rule" `isSuffixOf`) args
    when (null refnFilenames)
        $ putStrLn "Warning: no *.rule file is given."

    specPair  <- pairWithContents specFilename
    refnPairs <- mapM pairWithContents refnFilenames

    [spec ] <- runCompEIO (readSpec specPair)
    [refns] <- runCompEIO (concat <$> mapM readRuleRefn refnPairs)

    outSpecs <- runCompEIO (conjureRefn True spec refns)

    writeSpecs specFilename "refn" outSpecs

