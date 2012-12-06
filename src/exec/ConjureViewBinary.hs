{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Language.E

import System.Environment ( getArgs )
import qualified Data.ByteString as ByteString


main :: IO ()
main = do
    args <- getArgs

    forM_ args $ \ arg -> case fileType arg of
        Essence -> do
            con  <- ByteString.readFile arg
            case decode con of
                Left e -> error e
                Right x -> printPretty (x :: Spec)
        RuleRefn -> do
            con  <- ByteString.readFile arg
            case decode con of
                Left e -> error e
                Right x -> mapM_ printPretty (x :: [RuleRefn])
        RuleRepr -> do
            con  <- ByteString.readFile arg
            case decode con of
                Left e -> error e
                Right x -> printPretty (x :: RuleRepr)
        None -> return ()


data FileType = Essence | RuleRefn | RuleRepr | None

fileType :: FilePath -> FileType
fileType fp
    | ".essence.binary" `isSuffixOf` fp = Essence
    | ".rule.binary"    `isSuffixOf` fp = RuleRefn
    | ".repr.binary"    `isSuffixOf` fp = RuleRepr
    | otherwise                  = None

