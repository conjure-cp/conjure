module Main where

import Language.E
import Language.E.Pipeline.ReadIn

import System.Environment ( getArgs )
import qualified Data.ByteString as ByteString


main :: IO ()
main = do
    args <- getArgs

    forM_ args $ \ arg -> case fileType arg of
        Essence -> do
            pair <- pairWithContents arg
            x    <- handleInIOSingle =<< runCompEIOSingle
                    "Parsing problem specification"
                    (readSpec pair)
            ByteString.writeFile (arg ++ ".binary") (encode x)
        RuleRefn -> do
            pair <- pairWithContents arg
            x    <- handleInIOSingle =<< runCompEIOSingle
                    "Parsing rules"
                    (readRuleRefn pair)
            ByteString.writeFile (arg ++ ".binary") (encode x)
        RuleRepr -> do
            pair <- pairWithContents arg
            x    <- handleInIOSingle =<< runCompEIOSingle
                    "Parsing rules"
                    (readRuleRepr pair)
            ByteString.writeFile (arg ++ ".binary") (encode x)
        None -> return ()


data FileType = Essence | RuleRefn | RuleRepr | None

fileType :: FilePath -> FileType
fileType fp
    | ".essence" `isSuffixOf` fp = Essence
    | ".rule"    `isSuffixOf` fp = RuleRefn
    | ".repr"    `isSuffixOf` fp = RuleRepr
    | otherwise                  = None

