module Main where

import System.Environment (getArgs)

import Language.E
import Language.E.Pipeline.ReadIn
import Language.E.Pipeline.ConjureAll (conjureAllDriver)


main :: IO ()
main = do
    args <- getArgs

    specFilename <- case filter (".essence" `isSuffixOf`) args of
                        [t] -> return t
                        _   -> error "Only 1 *.essence file."

    let refnFilenames = filter (".rule" `isSuffixOf`) args
    when (null refnFilenames)
        $ putStrLn "Warning: no *.rule file is given."

    let reprFilenames = filter (".repr" `isSuffixOf`) args
    when (null reprFilenames)
        $ putStrLn "Warning: no *.repr file is given."

    specPair  <- pairWithContents specFilename
    reprPairs <- mapM pairWithContents reprFilenames
    refnPairs <- mapM pairWithContents refnFilenames

    [spec ]   <- runCompEIO (readSpec specPair)
    [refns]   <- runCompEIO (concat <$> mapM readRuleRefn refnPairs)
    [reprs]   <- runCompEIO (mapM readRuleRepr reprPairs)

    conjureAllDriver (dropExtEssence specFilename) reprs refns spec

