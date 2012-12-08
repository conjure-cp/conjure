{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Paths_conjure_cp ( getBinDir )
import System.Environment ( getArgs )

import Language.E
import Language.E.Pipeline.ReadIn
import Language.E.Pipeline.ConjureAll
import Language.E.Pipeline.Driver


rulesdbLoc :: IO FilePath
rulesdbLoc = liftM (++ "/conjure.rulesdb") getBinDir

main :: IO ()
main = do
    args <- getArgs

    specFilename <- case filter (".essence" `isSuffixOf`) args of
                        [t] -> return t
                        _   -> error "Only 1 *.essence file."

    (ruleReprs, ruleRefns) :: RulesDB <- decodeFromFile =<< rulesdbLoc

    specPair  <- pairWithContents specFilename

    spec  <- handleInIOSingle =<< runCompEIOSingle
                "Parsing problem specification"
                (readSpec specPair)

    driverConjure
        conjureAllPure (dropExtEssence specFilename)
        ruleReprs ruleRefns spec

