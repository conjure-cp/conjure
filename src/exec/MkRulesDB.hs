{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Language.E
import Language.E.Pipeline.ReadIn

import System.Environment ( getArgs )
import qualified Data.ByteString as ByteString
import Paths_conjure_cp


main :: IO ()
main = do
    args <- getArgs
    outfile <- liftM (++ "/conjure.rulesdb") getBinDir
    rulesdb <- liftM mconcat $ forM args $ \ arg -> case fileType arg of
            RuleRefn -> do
                pair <- pairWithContents arg
                x    <- handleInIOSingle =<< runCompEIOSingle
                        "Parsing rules"
                        (readRuleRefn pair)
                return ([],x)
            RuleRepr -> do
                pair <- pairWithContents arg
                x    <- handleInIOSingle =<< runCompEIOSingle
                        "Parsing rules"
                        (readRuleRepr pair)
                return ([x],[])
            None -> return ([],[])
    ByteString.writeFile outfile (encode (rulesdb :: RulesDB))


data FileType = RuleRefn | RuleRepr | None

fileType :: FilePath -> FileType
fileType fp
    | ".rule"    `isSuffixOf` fp = RuleRefn
    | ".repr"    `isSuffixOf` fp = RuleRepr
    | otherwise                  = None

