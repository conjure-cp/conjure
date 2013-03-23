#!/usr/bin/env runhaskell

-- Quick and dirty script, to run conjure on a single *.essence file, and
-- several *.param files.
--
-- Usage: `scripts/run/conjure-solve-all.hs *` in a directory with said
-- files.
--
-- Will generate all models for the spec, so might take a while.
-- Change `mode` (see below) with something else (like "compact") for
-- a faster version.

import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process

mode :: String
mode = "df"

main :: IO ()
main = do
    args <- getArgs

    let essence =
            case filter (".essence" `isSuffixOf`) args of
                [x] -> x
                _   -> error "Give only one Essence file"
    let params  =
            filter (".param"   `isSuffixOf`) args

    when (null params)
         (error "No *.param files given.")


    putStrLn "Running Conjure..."
    run [ unwords
            [ "conjure"
            , "--mode"       , mode
            , "--in-essence" , essence
            ]
        ]
    let outputDir = (reverse . drop 8 . reverse) essence

    putStrLn "Translating parameters..."
    eprimes <- filter (".eprime" `isSuffixOf`)
                <$>
               getDirectoryContents outputDir


    putStrLn "Refining params..."
    run [ unwords
            [ "conjure"
            , "--mode refineParam"
            , "--in-essence"       , essence
            , "--in-eprime"        , outputDir </> eprime
            , "--in-essence-param" , param
            , "--out-eprime-param" , outputDir </> dropExt eprime ++ "-" ++ dropExt param ++ ".param"
            ]
        | eprime <- eprimes
        , param <- params
        ]


    putStrLn "Solving..."
    run [ unwords
            [ "savilerow"
            , "-in-eprime"    , outputDir </> eprime
            , "-in-param"     , filenameTemplate ".param"
            , "-out-minion"   , filenameTemplate ".minion"
            , "-out-solution" , filenameTemplate ".SR.solution"
            ]
        | eprime <- eprimes
        , param <- params
        , let filenameTemplate extension = outputDir </> dropExt eprime ++ "-" ++ dropExt param ++ extension
        ]


    putStrLn "Converting solutions..."
    run [ unwords
            [ "conjure-solution"
            , outputDir </> eprime                                                  -- eprime
            , filenameTemplate ".SR.solution"                                       -- solution
            , essence                                                               -- essence
            , param                                                                 -- essence param file
            , outputDir </> dropExt eprime ++ "-" ++ dropExt param ++ ".param"      -- eprime param file
            ]
        | eprime <- eprimes
        , param <- params
        , let filenameTemplate extension = outputDir </> dropExt eprime ++ "-" ++ dropExt param ++ extension
        ]


    putStrLn "Finished."


run :: [String] -> IO ()
run [] = return ()
run (command:rest) = do
    putStrLn command
    handle <- runCommand command
    exitcode <- waitForProcess handle
    case exitcode of
        ExitSuccess   -> run rest
        ExitFailure _ -> exitWith exitcode


dropExt :: String -> String
dropExt = intercalate "." . reverse . drop 1 . reverse . splitOn "."


