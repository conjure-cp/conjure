module Main where

import Control.Applicative
import Control.Monad ( forM )
import Data.List ( isSuffixOf )
import System.Directory ( removeFile )
import System.Environment ( getArgs )

import Language.EssenceParsers ( pSpec )
import ParsecUtils ( parseFromFile )
import Phases.QuanRename ( quanRenameIO )
import UniqueSupply ( resetUniqueInt )


main :: IO ()
main = do
    rmIn  <- filter ("--rm" `isSuffixOf`) <$> getArgs
    args  <- filter (".essence" `isSuffixOf`) <$> getArgs
    specs <- forM args $ \ nm -> resetUniqueInt >> parseFromFile pSpec langlinePre nm id >>= quanRenameIO
    case specs of
        [a,b]  -> case rmIn of [] -> if a == b then putStrLn "Same."
                                               else putStrLn "Not same."
                               _  -> if a == b then removeFile (args !! 1)
                                               else return ()
        _      -> error "Expected two *.essence files."

langlinePre :: String -> String
langlinePre s = unlines $ "language Essence 2.0" : drop 1 (lines s)
