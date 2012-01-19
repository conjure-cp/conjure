
-- | the representation selection phase of Conjure

module Main where

import Control.Monad ( when )
import Control.Monad.Error ( runErrorT )
import Data.List ( isSuffixOf )
import System.Environment ( getArgs )
import System.FilePath ( dropExtension )
import System.Directory ( createDirectoryIfMissing )

import Language.EssenceParsers ( pSpec, pRuleRepr )
import Language.EssencePrinters ( prSpec )
import ParsecUtils ( parseFromFile )
import Phases.Repr ( applyToSpec )
import PrintUtils ( render )
import Utils ( padLeft )


main :: IO ()
main = do
    args <- getArgs
    specFilename <- case filter (".essence" `isSuffixOf`) args of
                        [t] -> return t
                        _   -> error "Only 1 *.essence file."
    spec  <- parseFromFile pSpec id specFilename id
    reprs <- mapM (\ r -> parseFromFile pRuleRepr id r id ) $ filter (".repr" `isSuffixOf`) args

    when (null reprs) $ putStrLn "Warning: no *.repr file is given."

    specs <- runErrorT $ applyToSpec reprs spec

    let dirName = dropExtension specFilename ++ "-repr"
    createDirectoryIfMissing True dirName

    case specs of
        Left err -> error err
        Right ss -> flip mapM_ (zip [(1::Int)..] ss) $ \ (i,s) -> do
            let outFilename = dirName ++ "/" ++ padLeft '0' 6 (show i) ++ ".essence"
            putStrLn $ "Outputting: " ++ outFilename
            writeFile outFilename $ render prSpec s
