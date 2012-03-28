module Main where

import Control.Monad ( forM_, when )
import Control.Monad.Error ( runErrorT )
import Control.Monad.Writer ( runWriter )
import Data.List ( isSuffixOf )
import System.Directory ( createDirectoryIfMissing )
import System.Environment ( getArgs )
import System.FilePath ( dropExtension )

import Language.Essence.RuleRefn ( RuleRefn, callRefn, refnFilename )

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
    refns <- mapM (\ r -> parseFromFile parse id r (\ i -> i {refnFilename = r} ) ) $ filter (".rule" `isSuffixOf`) args

    when (null refns) $ putStrLn "Warning: no *.rule file is given."

    -- print spec

    let (mspecs, logs) = runWriter $ runErrorT $ callRefn refns spec

    mapM_ print logs

    let dirName = dropExtension specFilename ++ "-refn"
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

