module Main where

import Control.Monad ( forM_, when )
import Control.Monad.Error ( runErrorT )
import Control.Monad.Writer ( runWriterT )
import Data.List ( isSuffixOf )
import System.Directory ( createDirectoryIfMissing )
import System.Environment ( getArgs )
import System.FilePath ( dropExtension )

import Language.Essence.RuleRefn ( callRefn )
import Language.Essence.Phases.ReadIn ( readInSpec, readInRefn )

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

    (mspecs, logs) <- runWriterT $ runErrorT $ do
        spec  <- readInSpec specFilename
        refns <- mapM readInRefn refnFilenames
        callRefn refns spec

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
