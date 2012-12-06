{-# LANGUAGE OverloadedStrings #-}

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
        RulesDB -> do
            con  <- ByteString.readFile arg
            case decode con of
                Left e -> error e
                Right x -> do
                    let (reprCnt, refnCnt) = (length *** length) (x :: RulesDB)
                    printPretty $ vcat
                        [          pretty arg     <+> "is a valid Conjure rules database file"
                        , nest 4 $ pretty reprCnt <+> "representation selection rules"
                        , nest 4 $ pretty refnCnt <+> "expression refinement rules"
                        ]
        None -> return ()


data FileType = Essence | RuleRefn | RuleRepr | RulesDB | None

fileType :: FilePath -> FileType
fileType fp
    | ".essence.binary" `isSuffixOf` fp = Essence
    | ".rule.binary"    `isSuffixOf` fp = RuleRefn
    | ".repr.binary"    `isSuffixOf` fp = RuleRepr
    | ".rulesdb"        `isSuffixOf` fp = RulesDB
    | otherwise                  = None

