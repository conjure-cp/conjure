module Test.Language.EssenceParsePrintFromFile where


import Data.List.Split
import Test.HUnit ( Test(..), test )

import Test.Language.EssenceParsePrintCommon
import Utils ( strip )


-- parses tests from a given String (possibly containing contents of a file)
-- a single test looks like: "*** Keyword Arg1 [~~ Arg2]"
-- possible keywords are: ShouldParse/1, NoParse/1, ShouldParseTo/2, ParsePrint/2
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
            ("ShouldParse"  , [i]  ) -> cmdShouldParse i
            ("NoParse"      , [i]  ) -> cmdNoParse i
            ("ShouldParseTo", [i,j]) -> cmdShouldParseTo i j
            ("ParsePrint"   , [i,j]) -> cmdParsePrint i j
            ("ParsePrintIso", [i]  ) -> cmdParsePrintIso i
            ("Eval"         , [i,j]) -> cmdEval i j
            _     -> error $ "unknown line format: " ++ line

parseLine :: String -> (String,[String])
parseLine s = case words s of
    (a:_) -> (a, map strip $ splitOn "~~" $ drop (length a) s )
    _     -> error $ "unknown line format: " ++ s
