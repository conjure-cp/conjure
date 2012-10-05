{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List ( isSuffixOf )
import System.Environment ( getArgs )
import System.IO.Unsafe ( unsafeInterleaveIO )

import Language.E
import Language.E.Pipeline.ReadIn
import Language.E.Pipeline.ConjureRepr ( conjureRepr )
import Language.E.Pipeline.ConjureRefn ( conjureRefn )
import Language.E.Pipeline.Groom ( groomSpec )


main :: IO ()
main = do
    args <- getArgs

    specFilename <- case filter (".essence" `isSuffixOf`) args of
                        [t] -> return t
                        _   -> error "Only 1 *.essence file."

    let refnFilenames = filter (".rule" `isSuffixOf`) args
    when (null refnFilenames)
        $ error "Warning: no *.rule file is given."

    let reprFilenames = filter (".repr" `isSuffixOf`) args
    when (null reprFilenames)
        $ error "Warning: no *.repr file is given."

    specPair  <- pairWithContents specFilename
    reprPairs <- mapM pairWithContents reprFilenames
    refnPairs <- mapM pairWithContents refnFilenames

    [spec ]   <- runCompEIO (readSpec specPair)
    [refns]   <- runCompEIO (concat <$> mapM readRuleRefn refnPairs)
    [reprs]   <- runCompEIO (mapM readRuleRepr reprPairs)

    outSpecs  <- loop reprs refns spec

    writeSpecs (dropExtEssence specFilename) "conjure" outSpecs


data Phase
    = Repr
    | Refn

loop :: [RuleRepr] -> [RuleRefn] -> Spec -> IO [Spec]
loop reprs refns = go Repr
    where
        go :: Phase -> Spec -> IO [Spec]
        go Repr s = do
            generateds <- runCompEIO (conjureRepr False s reprs)
            let lgenerateds = length generateds
            when (lgenerateds > 1) $ putStrLn $ "multiple alternatives after repr: " ++ show lgenerateds
            if null generateds
                then runCompEIO (groomSpec s)
                else concatMapM (unsafeInterleaveIO . go Refn) generateds
        go Refn s = do
            generateds <- runCompEIO (conjureRefn False s refns)
            let lgenerateds = length generateds
            when (lgenerateds > 1) $ putStrLn $ "multiple alternatives after refn: " ++ show lgenerateds
            if null generateds
                then runCompEIO (groomSpec s)
                else concatMapM (unsafeInterleaveIO . go Repr) generateds

