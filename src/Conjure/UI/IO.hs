{-# LANGUAGE OverloadedStrings #-}

module Conjure.UI.IO where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Parser
import Conjure.Language.Pretty
import Language.E.Definition

-- text
import qualified Data.Text as T


readModelFromFile :: FilePath -> IO Model
readModelFromFile fp = do
    pair <- pairWithContents fp
    return (readModel pair)

readModelFromStdIn :: IO Model
readModelFromStdIn = do
    stdin <- getContents
    return (readModel ("<stdin>", T.pack stdin))

readModelPreambleFromFile :: FilePath -> IO Model
readModelPreambleFromFile fp = do
    pair <- pairWithContents fp
    return (readModelPreamble pair)

readModel :: (FilePath, Text) -> Model
readModel (fp, con) =
    case runLexerAndParser parseModel fp con of
        Left  e -> userErr e
        Right x -> x


readModelPreamble :: (FilePath, Text) -> Model
readModelPreamble (fp,con) =
    case runLexerAndParser parseModel fp (onlyPreamble con) of
        Left  e -> userErr e
        Right x -> x
    where
        stripComments = T.unlines . map (T.takeWhile (/= '$')) . T.lines
        discardAfter t = fst . T.breakOn t
        onlyPreamble
            = discardAfter "maximising"
            . discardAfter "maximizing"
            . discardAfter "minimising"
            . discardAfter "minimizing"
            . discardAfter "such that"
            . stripComments


fixRulename :: String -> String
fixRulename = intercalate "/" . dropWhile (/="rules") . splitOn "/"


readRuleRefn :: (FilePath, Text) -> [RuleRefn]
readRuleRefn (fp,con) =
    case runLexerAndParser (parseRuleRefn $ stringToText $ fixRulename fp) fp con of
        Left  e -> userErr e
        Right x -> x


readRuleRepr :: (FilePath, Text) -> RuleRepr
readRuleRepr (fp,con) =
    case runLexerAndParser (parseRuleRepr $ stringToText $ fixRulename fp) fp con of
        Left  e -> userErr e
        Right x -> x


writeModel :: Maybe FilePath -> Model -> IO ()
writeModel Nothing   spec = putStrLn     (renderNormal spec)
writeModel (Just fp) spec = writeFile fp (renderNormal spec)

writeModels :: FilePath -> String -> [Model] -> IO ()
writeModels base tag specs = do
    let numbers = map (padShowInt 4) [ (1 :: Int) .. ]
    forM_ (zip numbers specs) $ \ (i, spec) -> do
        let outDirname  = base ++ "-" ++ tag
        let outFilename = base ++ "-" ++ tag ++ "/" ++ i ++ ".essence"
        createDirectoryIfMissing True outDirname
        writeModel (Just outFilename) spec
        putStrLn $ "[created file] " ++ outFilename

