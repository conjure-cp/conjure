{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List ( isSuffixOf )
import System.Environment ( getArgs )

import Language.E
import Language.E.Pipeline.ReadIn
import Language.E.Pipeline.ConjureRepr ( conjureRepr )
import Language.E.Pipeline.ConjureRefn ( conjureRefn )
import Language.E.Pipeline.NoGuards ( conjureNoGuards )
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )


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
        go Repr s = do
            putStrLn "starting Repr"
            generateds <- runCompEIO (conjureRepr s reprs)
            putStrLn $ "Repr: returning " ++ show (length generateds)
            if null generateds
                then groomSpec s
                else concat <$> mapM (go Refn) generateds
        go Refn s = do
            putStrLn "starting Refn"
            generateds <- runCompEIO (conjureRefn s refns)
            putStrLn $ "Refn: returning " ++ show (length generateds)
            if null generateds
                then groomSpec s
                else concat <$> mapM (go Repr) generateds

groomSpec :: Spec -> IO [Spec]
groomSpec = runCompEIO . pipeline
    where pipeline = conjureNoGuards >=> return . atMostOneSuchThat
                                     >=> return . langEPrime
          langEPrime (Spec _ xs) = Spec ("ESSENCE'", [1,0]) xs

