{-# LANGUAGE ViewPatterns #-}

module Test.Language.Unit.FromFile where

import Control.Monad ( join )
import Data.List.Split ( splitOn )
import Data.Maybe ( maybeToList )
import Test.HUnit ( Test(..), test )
import Text.Parsec.Char ( noneOf, string, spaces )

import Language.Essence
import Language.EssenceParsers
import ParsecUtils
import Test.Language.Unit.Common
import Utils ( strip )


-- parses tests from a given String (possibly containing contents of a file)
-- a single test looks like: "*** Keyword Arg1 [~~ Arg2]"
-- possible keywords are: ShouldParse/1, NoParse/1, ShouldParseTo/2,
--                        ParsePrint/2, ParsePrintIso/1, Eval/2, TypeOf/2
allTests :: String -> (Test, Int)
allTests file = (test (map toTest ls), length ls)
    where
        isComment :: String -> Bool
        isComment ('#':_) = True
        isComment (' ':cs) = isComment cs
        isComment _ = False

        ls :: [String]
        ls = map strip
            $ filter (not . null)
            $ splitOn "***"
            $ unlines
            $ filter (not . isComment)
            $ filter (not . null)
            $ lines file

        toTest :: String -> Test
        toTest line = case parseLine line of
            ("ShouldParse"  , _bindings, [i]  ) -> cmdShouldParse i
            ("NoParse"      , _bindings, [i]  ) -> cmdNoParse i
            ("ShouldParseTo", _bindings, [i,j]) -> cmdShouldParseTo i j
            ("ParsePrint"   , _bindings, [i,j]) -> cmdParsePrint i j
            ("ParsePrintIso", _bindings, [i]  ) -> cmdParsePrintIso i
            ("Eval"         ,  bindings, [i,j]) -> cmdEval bindings i j
            ("TypeOf"       ,  bindings, [i,j,k]) -> cmdTypeOf bindings i j k
            _     -> error $ "unknown line format: " ++ line


parseLine :: String -> (String,[Binding],[String])
parseLine (strip -> s) = case parseEither pLine s of
    Left msg        -> error $ "unknown line format: " ++ s ++ "\n\n" ++ msg
    Right (c,bs,as) -> (c,join (maybeToList bs), as)
    where
        pLine :: Parser (String, Maybe [Binding], [String])
        pLine = do
            cmd      <- pStrings $ words "ShouldParseTo ShouldParse NoParse ParsePrintIso ParsePrint Eval TypeOf"
            spaces
            bindings <- optionMaybe $ braces $ fmap fst $ pTopLevels
            spaces
            args     <- pArg `sepBy1` reservedOp "~~"
            eof
            return (cmd, bindings, map strip args)
            where
                pStrings :: [String] -> Parser String
                pStrings ss = choiceTry $ map string ss

                pArg :: Parser String
                pArg = many1 (noneOf "~")
