module Main where

import Control.Monad ( forM_, when )
import Control.Monad.Error ( runErrorT )
import Control.Monad.Writer ( runWriter )
import Data.List ( isSuffixOf )
import System.Directory ( createDirectoryIfMissing )
import System.Environment ( getArgs )
import System.FilePath ( dropExtension )

import Language.Essence.RuleRepr ( RuleRepr, callRepr, reprFilename )

import ParsecUtils ( parseFromFile )
import ParsePrint ( parse, pretty )
import Utils ( padLeft )



main :: IO ()
main = do
    args <- getArgs
    specFilename <- case filter (".essence" `isSuffixOf`) args of
                        [t] -> return t
                        _   -> error "Only 1 *.essence file."
    spec  <- {-quanRenameIO =<< -}parseFromFile parse langlinePre specFilename id
    reprs <- mapM (\ r -> parseFromFile parse id r (\ i -> i {reprFilename = r} ) ) $ filter (".repr" `isSuffixOf`) args

    when (null reprs) $ putStrLn "Warning: no *.repr file is given."

    let (mspecs, logs) = runWriter $ runErrorT $ callRepr reprs spec

    mapM_ print logs

    let dirName = dropExtension specFilename ++ "-repr"
    createDirectoryIfMissing True dirName

    case mspecs of
        Left err -> error (show err)
        Right specs ->
            forM_ (zip [(1::Int)..] specs) $ \ (i,s) -> do
                let outFilename = dirName ++ "/" ++ padLeft '0' 6 (show i) ++ ".essence"
                putStrLn outFilename
                writeFile outFilename $ show $ pretty s

langlinePre :: String -> String
langlinePre s = unlines $ "language Essence 2.0" : drop 1 (lines s)

