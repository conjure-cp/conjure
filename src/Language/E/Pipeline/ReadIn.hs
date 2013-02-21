{-# LANGUAGE OverloadedStrings #-}

module Language.E.Pipeline.ReadIn where

import Language.E

import Data.Text ( breakOn, pack )
import System.Directory ( createDirectoryIfMissing )


readSpecFromFile :: FilePath -> IO Spec
readSpecFromFile fp = do
    let errorMsg = "Error while parsing file."
    pair <- pairWithContents fp
    handleInIOSingle =<<
        runCompEIOSingle errorMsg (readSpec pair)


readSpecFromStdIn :: IO Spec
readSpecFromStdIn = do
    let errorMsg = "Error while parsing file."
    stdin <- getContents
    handleInIOSingle =<<
        runCompEIOSingle errorMsg (readSpec ("<STDIN>", pack stdin))


readSpec :: MonadConjure m
    => (FilePath, Text)
    -> m Spec
readSpec (fp,con) =
    case runLexerAndParser parseSpec fp con of
        Left  e -> err ErrFatal e
        Right x -> initialiseSpecState x >> return x

readSpecPreamble :: MonadConjure m
    => (FilePath, Text)
    -> m Spec
readSpecPreamble (fp,con) =
    case runLexerAndParser parseSpec fp (onlyPreamble con) of
        Left  e -> err ErrFatal e
        Right x -> initialiseSpecState x >> return x
    where
        discardAfter t = fst . breakOn t
        onlyPreamble
            = discardAfter "maximising"
            . discardAfter "maximizing"
            . discardAfter "minimising"
            . discardAfter "minimizing"
            . discardAfter "such that"


fixRulename :: String -> String
fixRulename = intercalate "/" . dropWhile (/="rules") . splitOn "/"


readRuleRefn :: MonadConjure m
    => (FilePath, Text)
    -> m [RuleRefn]
readRuleRefn (fp,con) =
    case runLexerAndParser (parseRuleRefn $ stringToText $ fixRulename fp) fp con of
        Left  e -> err ErrFatal e
        Right x -> return x


readRuleRepr :: MonadConjure m
    => (FilePath, Text)
    -> m RuleRepr
readRuleRepr (fp,con) =
    case runLexerAndParser (parseRuleRepr $ stringToText $ fixRulename fp) fp con of
        Left  e -> err ErrFatal e
        Right x -> return x


writeSpec :: FilePath -> Spec -> IO ()
writeSpec fp spec = writeFile fp (renderPretty spec)


writeSpecs :: FilePath -> String -> [Spec] -> IO ()
writeSpecs base tag specs = do
    let numbers = map (padShowInt 4) [ (1 :: Int) .. ]
    forM_ (zip numbers specs) $ \ (i, spec) -> do
        let outDirname  = base ++ "-" ++ tag
        let outFilename = base ++ "-" ++ tag ++ "/" ++ i ++ ".essence"
        createDirectoryIfMissing True outDirname
        writeSpec outFilename spec
        putStrLn $ "[created file] " ++ outFilename


dropExtEssence :: String -> String
dropExtEssence = reverse . drop 8 . reverse
