
-- | the representation selection phase of Conjure

module Main where

import Control.Monad ( forM_, when )
import Data.List ( isSuffixOf )
import System.Environment ( getArgs )
import System.FilePath ( dropExtension )
import System.Directory ( createDirectoryIfMissing )
import Data.Conduit.List as Con

import Language.EssenceParsers ( pSpec, pRuleRepr )
import Language.EssencePrinters ( prSpec )
import Language.EssenceLexerP ( parseFromFile )
import Phases.Repr ( callRepr )
import PrintUtils ( render )
import Utils ( padLeft )


main :: IO ()
main = do
    args <- getArgs
    specFilename <- case filter (".essence" `isSuffixOf`) args of
                        [t] -> return t
                        _   -> error "Only 1 *.essence file."
    spec  <- parseFromFile pSpec langlinePre specFilename id
    reprs <- mapM (\ r -> parseFromFile (pRuleRepr r) id r id ) $ filter (".repr" `isSuffixOf`) args

    when (null reprs) $ putStrLn "Warning: no *.repr file is given."

    specs <- callRepr reprs spec

    let dirName = dropExtension specFilename ++ "-repr"
    createDirectoryIfMissing True dirName

    let
        perFile :: Int -> Spec -> IO ()
        perFile i s = do
            let outFilename = dirName ++ "/" ++ padLeft '0' 6 (show i) ++ ".essence"
            putStrLn outFilename
            writeFile outFilename $ render prSpec s

    runResourceT $ Con.mapM_ perFile $$ sourceList (zip [(1::Int)..] specs)

langlinePre :: String -> String
langlinePre s = unlines $ "language Essence 2.0" : drop 1 (lines s)
