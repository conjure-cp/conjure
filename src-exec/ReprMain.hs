module Main where

import Control.Monad ( forM_, when, zipWithM )
import Control.Monad.Error ( runErrorT )
import Control.Monad.Writer ( runWriter )
import Data.List ( isSuffixOf )
import System.Directory ( createDirectoryIfMissing )
import System.Environment ( getArgs )
import System.FilePath ( dropExtension )
import qualified Data.Text.Lazy.IO as T

import Language.Essence.Phases.PhaseRepr ( callRepr )
import Language.Essence.Phases.ReadIn ( readIn )

import ParsePrint ( pretty )
import Utils ( padLeft )



main :: IO ()
main = do
    args <- getArgs
    specFilename <- case filter (".essence" `isSuffixOf`) args of
                        [t] -> return t
                        _   -> error "Only 1 *.essence file."
    let reprFilenames = filter (".repr" `isSuffixOf`) args
    when (null reprFilenames) $ putStrLn "Warning: no *.repr file is given."

    specFile  <- T.readFile specFilename
    reprFiles <- mapM T.readFile reprFilenames
    let (mspecs, logs) = runWriter $ runErrorT $ do
            spec  <- readIn (Just specFilename) specFile
            reprs <- zipWithM readIn (map Just reprFilenames) reprFiles
            callRepr Nothing reprs spec

    mapM_ print logs

    case mspecs of
        Left err -> error (show err)
        Right specs -> do
            let dirName = dropExtension specFilename ++ "-repr"
            createDirectoryIfMissing True dirName
            forM_ (zip [(1::Int)..] specs) $ \ (i,s) -> do
                let outFilename = dirName ++ "/" ++ padLeft '0' 6 (show i) ++ ".essence"
                putStrLn outFilename
                writeFile outFilename $ show $ pretty s
