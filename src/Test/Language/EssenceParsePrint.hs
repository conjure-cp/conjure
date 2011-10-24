{-# LANGUAGE TemplateHaskell #-}

module Test.Language.EssenceParsePrint where


import Control.Applicative
import Data.List.Split
import Data.Maybe
import Test.HUnit ( Test(..), (~:), (~=?), assertBool, test )

import Language.Essence ( Expr(..) )
import Language.EssenceParsers ( pExpr )
import Language.EssencePrinters ( prExpr )
import ParsecUtils ( eof, parseMaybe )
import PrintUtils ( render )
import Utils ( maybeRead, strip )


-- parses tests from a given String (possibly containing contents of a file)
-- a single test looks like: "*** Keyword Arg1 [~~ Arg2]"
-- possible keywords are: ShouldParse/1, NoParse/1, ShouldParseTo/2, ParsePrint/2
allTests :: String -> Test
allTests file = test $ map toTest ls
    where
        isComment :: String -> Bool
        isComment ('#':_) = True
        isComment _ = False

        ls :: [String]
        ls = map strip
            $ filter (not . null)
            $ splitOn "***"
            $ unlines
            $ filter (not . isComment)
            $ lines file

        toTest :: String -> Test
        toTest line = case firstWord line of
            ("ShouldParse"  , rest) -> TestCase $ assertBool "ShouldParse" $ isJust $ parseMaybe (pExpr <* eof) (strip rest)
            ("NoParse"      , rest) -> "NoParse" ~: Nothing ~=? parseMaybe (pExpr <* eof) (strip rest)
            ("ShouldParseTo", rest) -> case splitOn "~~" rest of
                [a,b] -> "ShouldParseTo" ~: maybeRead (strip b) ~=? parseMaybe (pExpr <* eof) (strip a)
                _     -> error ("cannot parse line: " ++ line)
            ("ParsePrint"   , rest) -> case splitOn "~~" rest of
                [a,b] -> parsePrint (strip a) (strip b)
                _     -> error ("cannot parse line: ParsePrint " ++ rest)
            _ -> error ("unknown line format: " ++ line)

        parsePrint :: String -> String -> Test
        parsePrint s x = test [ TestCase $ assertBool "[parse]" $ isJust $ sParsed
                                  , "[parse & print]" ~: sParsedPrinted ~=? Just s
                                  , "[print & parse]" ~: maybeRead x ~=? xPrintedParsed
                                  ]
            where
                sParsed :: Maybe Expr
                sParsed = parseMaybe pExpr s

                sParsedPrinted :: Maybe String
                sParsedPrinted = render <$> (prExpr =<< sParsed)

                xPrinted :: Maybe String
                xPrinted = render <$> (prExpr =<< maybeRead x)

                xPrintedParsed :: Maybe Expr
                xPrintedParsed = parseMaybe pExpr =<< xPrinted


firstWord :: String -> (String,String)
firstWord s = case words s of
    (a:_) -> (a, strip (drop (length a) s))
    _     -> error "firstWord"
