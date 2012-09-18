{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List ( isSuffixOf )
import System.Environment ( getArgs )

import Language.E
import Language.E.Pipeline.ReadIn
import Language.E.Pipeline.NoGuards ( conjureNoGuards )


main :: IO ()
main = do
    args <- getArgs

    specFilename <- case filter (".essence" `isSuffixOf`) args of
                        [t] -> return t
                        _   -> error "Only 1 *.essence file."

    spec    <- pairWithContents specFilename

    let
        (mgenerateds, glo) = runIdentity $ runCompE (conjureNoGuards spec)
        errors     = [ x  | (Left  x, _ ) <- mgenerateds ]
        generateds = [ x  | (Right x, _ ) <- mgenerateds ]
    print $ prettyLogs $ logs glo
    unless (null errors)
        $ error
        $ show
        $ prettyErrors "There were errors in at least one branch." errors

    -- putStrLn ""
    -- putStrLn "[ === Generated === ]"
    -- putStrLn ""
    -- mapM_ (putStrLn . renderPretty) generateds

    writeSpecs (dropExtEssence specFilename) generateds
