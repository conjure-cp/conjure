{-# LANGUAGE RecordWildCards #-}

module Conjure.ParserFuzz (tests) where

-- conjure

import Conjure.Prelude

-- base

-- tasty
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCaseSteps, assertEqual)

import Conjure.Language.Parser (runLexerAndParser)
import Conjure.Language.AST.ASTParser (runASTParser, parseProgram)
import Conjure.Language.NewLexer (runLexer, Reformable (reform), reformList)
import qualified Data.Text as T (pack, lines) 
import qualified Data.Text.Lazy as L  
import Data.ByteString.Char8(hPutStrLn, pack)
import Conjure.Language.AST.Reformer (Flattenable(flatten))
import Data.Algorithm.Diff (getDiff, getGroupedDiff)
import Data.Algorithm.DiffOutput (ppDiff)
import Data.ByteString (hPutStrLn)
import GHC.IO.Handle.FD (stderr)


tests :: IO TestTree
tests = do
    let baseDir = "tests"
    allFiles <- readFileIfExists "tests/allfiles.txt"
    let allFileList = lines $ fromMaybe "" allFiles
    contents <- mapM readFileIfExists allFileList
    let testCases = [testFile fp fd | (fp,Just fd) <-zip allFileList contents,False]
    return (testGroup "parse_fuzz" testCases)

testFile :: FilePath -> String -> TestTree
testFile fp fd = testCaseSteps (map (\ch -> if ch == '/' then '.' else ch) fp) $ \step ->  do
    step "Lexing"
    let usableFileData = concat (take 1000 . lines $ fd)
    let fText = T.pack usableFileData
    case runLexer $ fText of
      Left le -> assertFailure $ "Lexer failed in:" ++ fp
      Right ets -> do
                step "parsing"
                case runASTParser parseProgram ets of
                  Left pe -> assertFailure $ "Parser failed in:" ++ fp
                  Right pt -> do
                    step "RoundTripping"
                    let roundTrip = L.unpack $ reformList $ flatten pt
                    unless (roundTrip == usableFileData) $ do
                        let diff = getGroupedDiff (lines roundTrip) (lines usableFileData)
                        Data.ByteString.Char8.hPutStrLn stderr $ Data.ByteString.Char8.pack $ "===DIFF: " ++ fp
                        Data.ByteString.Char8.hPutStrLn stderr $ Data.ByteString.Char8.pack $ ppDiff diff
                        Data.ByteString.Char8.hPutStrLn stderr "===------------"
                        assertFailure $ "Failed to rebuild :" ++ fp