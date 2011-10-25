module Test.Language.EssenceParsePrintFromFile where


import Data.List.Split
import Test.HUnit ( Test(..), test )

import Test.Language.EssenceParsePrintCommon ( shouldParse, noParse, shouldParseTo, parsePrint)
import Utils ( strip )


-- parses tests from a given String (possibly containing contents of a file)
-- a single test looks like: "*** Keyword Arg1 [~~ Arg2]"
-- possible keywords are: ShouldParse/1, NoParse/1, ShouldParseTo/2, ParsePrint/2
allTests :: String -> Test
allTests file = test $ map toTest ls
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
        toTest line = case firstWord line of
            ("ShouldParse"  , rest) -> shouldParse (strip rest)
            ("NoParse"      , rest) -> noParse (strip rest)
            ("ShouldParseTo", rest) -> case splitOn "~~" rest of
                [a,b] -> shouldParseTo (strip a) (strip b)
                _     -> error ("cannot parse line: " ++ line)
            ("ParsePrint"   , rest) -> case splitOn "~~" rest of
                [a,b] -> parsePrint (strip a) (strip b)
                _     -> error ("cannot parse line: " ++ line)
            _ -> error "never here: Test.Language.EssenceParsePrint.allTests.toTest"

firstWord :: String -> (String,String)
firstWord "" = error "empty test file"
firstWord s = case words s of
    (a:_) -> (a, strip (drop (length a) s))
    _     -> error $ "unknown line format: " ++ s
