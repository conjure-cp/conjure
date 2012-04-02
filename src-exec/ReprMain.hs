module Main where

import Control.Monad ( forM_, when )
import Control.Monad.Error ( runErrorT )
import Control.Monad.Writer ( runWriterT )
import Data.List ( isSuffixOf )
import System.Directory ( createDirectoryIfMissing )
import System.Environment ( getArgs )
import System.FilePath ( dropExtension )

import Language.Essence.RuleRepr ( callRepr )
import Language.Essence.Phases.ReadIn ( readInSpec, readInRepr )

import ParsePrint ( pretty )
import Utils ( padLeft )



main :: IO ()
main = do
    args <- getArgs
    specFilename <- case filter (".essence" `isSuffixOf`) args of
                        [t] -> return t
                        _   -> error "Only 1 *.essence file."
    let reprFilenames = filter (".rule" `isSuffixOf`) args
    when (null reprFilenames) $ putStrLn "Warning: no *.rule file is given."

    (mspecs, logs) <- runWriterT $ runErrorT $ do
        spec  <- readInSpec specFilename
        reprs <- mapM readInRepr reprFilenames
        callRepr reprs spec

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
