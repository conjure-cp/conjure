{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.ReadIn where

import Language.E

import System.Directory ( createDirectoryIfMissing )


readSpec :: MonadConjure m
    => (FilePath, Text)
    -> m Spec
readSpec (fp,con) =
    case runLexerAndParser parseSpec fp con of
        Left  e -> err ErrFatal e
        Right x -> initialiseSpecState x >> return x


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


writeSpecs :: FilePath -> String -> [Spec] -> IO ()
writeSpecs base tag specs = do
    let padShow n i = let s = show i in replicate (n - length s) '0' ++ s
    let numbers = map (padShow 4) [ (1 :: Int) .. ]
    forM_ (zip numbers specs) $ \ (i, spec) -> do
        let outDirname  = base ++ "-" ++ tag
        let outFilename = base ++ "-" ++ tag ++ "/" ++ i ++ ".essence"
        createDirectoryIfMissing True outDirname
        writeFile outFilename $ renderPretty spec
        putStrLn $ "[created file] " ++ outFilename


dropExtEssence :: String -> String
dropExtEssence = reverse . drop 8 . reverse
