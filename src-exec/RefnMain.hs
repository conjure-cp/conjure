module Main where

import Control.Monad ( forM_, when, zipWithM )
import Control.Monad.Error ( runErrorT )
import Control.Monad.Writer ( runWriter )
import Data.List ( isSuffixOf )
import System.Directory ( createDirectoryIfMissing )
import System.Environment ( getArgs )
import System.FilePath ( dropExtension )

import Language.Essence.RuleRefn ( callRefn )
import Language.Essence.Phases.ReadIn ( readIn )

import ParsePrint ( pretty )
import Utils ( padLeft )



main :: IO ()
main = do
    args <- getArgs
    specFilename <- case filter (".essence" `isSuffixOf`) args of
                        [t] -> return t
                        _   -> error "Only 1 *.essence file."
    let refnFilenames = filter (".rule" `isSuffixOf`) args
    when (null refnFilenames) $ putStrLn "Warning: no *.rule file is given."

    specFile  <- readFile specFilename
    refnFiles <- mapM readFile refnFilenames
    let (mspecs, logs) = runWriter $ runErrorT $ do
            spec  <- readIn specFilename specFile
            refns <- zipWithM readIn refnFilenames refnFiles
            callRefn refns spec

    mapM_ print logs

    case mspecs of
        Left err -> error (show err)
        Right specs -> do
            let dirName = dropExtension specFilename ++ "-refn"
            createDirectoryIfMissing True dirName
            forM_ (zip [(1::Int)..] specs) $ \ (i,s) -> do
                let outFilename = dirName ++ "/" ++ padLeft '0' 6 (show i) ++ ".essence"
                putStrLn outFilename
                writeFile outFilename $ show $ pretty s
