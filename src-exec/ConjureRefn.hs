{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List ( isSuffixOf )
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
    -- when (null refnFilenames)
    --     $ error "Warning: no *.rule file is given."

    let reprFilenames = filter (".repr" `isSuffixOf`) args
    -- when (null reprFilenames)
    --     $ error "Warning: no *.repr file is given."

    spec    <- pairWithContents specFilename
    rules   <- mapM pairWithContents refnFilenames
    reprs   <- mapM pairWithContents reprFilenames

    let
        (mgenerateds, glo) = runIdentity $ runCompE (conjureRefn spec rules reprs)
        errors     = [ x  | (Left  x, _ ) <- mgenerateds ]
        generateds = [ x  | (Right x, _ ) <- mgenerateds ]
    putStrLn $ renderPretty $ prettyLogs $ logs glo
    unless (null errors)
        $ error
        $ show
        $ prettyErrors "There were errors in at least one branch." errors

    -- putStrLn ""
    -- putStrLn "[ === Generated === ]"
    -- putStrLn ""
    -- mapM_ (putStrLn . renderPretty) generateds

    writeSpecs (dropExtEssence specFilename) "refn" generateds
