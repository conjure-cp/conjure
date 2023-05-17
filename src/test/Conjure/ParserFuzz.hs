{-# LANGUAGE RecordWildCards #-}

module Conjure.ParserFuzz (tests) where

-- conjure

import Conjure.Prelude

-- base

-- tasty
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCaseSteps)

import Conjure.Language.AST.ASTParser (runASTParser, parseProgram)
import Conjure.Language.Lexer (runLexer, reformList)
import qualified Data.Text as T (pack, unpack) 
import qualified Data.Text.Lazy as L
import Data.ByteString.Char8(hPutStrLn, pack)
import Conjure.Language.AST.Reformer (flattenSeq)
import Data.Algorithm.Diff (getGroupedDiff)
import Data.Algorithm.DiffOutput (ppDiff)
import GHC.IO.Handle.FD (stderr)
import Shelly (run, shelly, silently)


tests :: IO TestTree
tests = do
    allFiles <-  shelly $ silently $ run  "git" ["ls-tree", "--full-tree",  "--name-only", "-r", "HEAD"] 
    let allFileList = lines $ T.unpack allFiles
    let testCases = testFile <$> allFileList
    return (testGroup "parse_fuzz" testCases)

testFile :: FilePath -> TestTree
testFile fp = testCaseSteps (map (\ch -> if ch == '/' then '.' else ch) fp) $ \step ->  do
    fd <- readFileIfExists fp
    step "Lexing"
    let usableFileData = concat (take 1000 . lines $ fromMaybe [] fd)
    let fText = T.pack usableFileData
    case runLexer fText (Just fp) of
      Left le -> assertFailure $ "Lexer failed in:" ++ fp
      Right ets -> do
                step "parsing"
                case runASTParser parseProgram ets of
                  Left _pe -> assertFailure $ "Parser failed in:" ++ fp
                  Right pt -> do
                    step "RoundTripping"
                    let roundTrip = L.unpack $ reformList $ flattenSeq pt
                    unless (roundTrip == usableFileData) $ do
                        let diff = getGroupedDiff (lines roundTrip) (lines usableFileData)
                        Data.ByteString.Char8.hPutStrLn stderr $ Data.ByteString.Char8.pack $ "===DIFF: " ++ fp
                        Data.ByteString.Char8.hPutStrLn stderr $ Data.ByteString.Char8.pack $ ppDiff diff
                        Data.ByteString.Char8.hPutStrLn stderr "===------------"
                        assertFailure $ "Failed to rebuild :" ++ fp