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
    when (null refnFilenames)
        $ error "Warning: no *.rule file is given."

    spec    <- pairWithContents specFilename
    rules   <- mapM pairWithContents refnFilenames

    let
        mgenerateds = runIdentity $ runCompE (toCore spec rules)
        logs        = mconcat [ ls | (_      , _, ls) <- mgenerateds ]
        errors      =         [ x  | (Left  x, _, _ ) <- mgenerateds ]
        generateds  =         [ x  | (Right x, _, _ ) <- mgenerateds ]
    print $ prettyLogs logs
    unless (null errors)
        $ error
        $ show
        $ prettyErrors "There were errors in at least one branch." errors

    putStrLn ""
    putStrLn "[ === Generated === ]"
    putStrLn ""

    mapM_ (print . pretty) generateds
