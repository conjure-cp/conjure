{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List ( isSuffixOf )
import System.Environment ( getArgs )

import Language.E
import Language.E.Pipeline.ToCore ( toCore )


main :: IO ()
main = do
    args <- getArgs

    specFilename <- case filter (".essence" `isSuffixOf`) args of
                        [t] -> return t
                        _   -> error "Only 1 *.essence file."

    let refnFilenames = filter (".rule" `isSuffixOf`) args
    -- when (null refnFilenames)
    --     $ error "Warning: no *.rule file is given."

    let reprFilenames = filter (".repr" `isSuffixOf`) args
    -- when (null reprFilenames)
    --     $ error "Warning: no *.repr file is given."

    spec    <- pairWithContents specFilename
    rules   <- mapM pairWithContents refnFilenames
    reprs   <- mapM pairWithContents reprFilenames

    let
        (mgenerateds, glo) = runIdentity $ runCompE (toCore spec rules reprs)
        errors     = [ x  | (Left  x, _ ) <- mgenerateds ]
        generateds = [ x  | (Right x, _ ) <- mgenerateds ]
    print $ prettyLogs $ logs glo
    unless (null errors)
        $ error
        $ show
        $ prettyErrors "There were errors in at least one branch." errors

    putStrLn ""
    putStrLn "[ === Generated === ]"
    putStrLn ""

    mapM_ (print . pretty) generateds
