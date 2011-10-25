{-# LANGUAGE TemplateHaskell #-}

module Test.Language.EssenceParsePrint where


import Control.Applicative
import Data.List.Split
import Data.Maybe
import Test.HUnit ( Test(..), (~:), (~=?), assertBool, test )

import Language.Essence ( Expr(..) )
import Language.EssenceParsers ( pExpr )
import Language.EssencePrinters ( prExpr )
import ParsecUtils ( Parser, eof, parseMaybe )
import PrintUtils ( render )
import Utils ( maybeRead, strip )


pExprEof :: Parser Expr
pExprEof = pExpr <* eof


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

        shouldParse :: String -> Test
        shouldParse s = test [ TestCase $ assertBool "ShouldParse" $ isJust sParsed
                             ]
            where
                sParsed :: Maybe Expr
                sParsed = parseMaybe pExprEof s

        noParse :: String -> Test
        noParse s = test [ TestCase $ assertBool "NoParse" $ isNothing sParsed
                         ]
            where
                sParsed :: Maybe Expr
                sParsed = parseMaybe pExprEof s

        shouldParseTo :: String -> String -> Test
        shouldParseTo s x = test [ TestCase $ assertBool ("ShouldParseTo.parse: " ++ s) $ isJust sParsed
                                 , TestCase $ assertBool ("ShouldParseTo.read: "  ++ x) $ isJust xRead
                                 , "ShouldParseTo.(s.parsed=x.read)" ~: sParsed ~=? xRead
                                 ]
            where
                sParsed :: Maybe Expr
                sParsed = parseMaybe pExprEof s

                xRead :: Maybe Expr
                xRead = maybeRead x

        parsePrint :: String -> String -> Test
        parsePrint s x = test [ TestCase $ assertBool ("ParsePrint.parse: " ++ s) $ isJust sParsed
                              , TestCase $ assertBool ("ParsePrint.read: "  ++ x) $ isJust xRead
                              , "ParsePrint.(parsed=read)" ~: sParsed ~=? xRead
                              , "ParsePrint.(s=x.printed)" ~: Just s  ~=? xPrinted
                              , "ParsePrint.(s=s.printed)" ~: Just s  ~=? sParsedPrinted
                              ]
            where
                sParsed :: Maybe Expr
                sParsed = parseMaybe pExprEof s

                xRead :: Maybe Expr
                xRead = maybeRead x

                sParsedPrinted :: Maybe String
                sParsedPrinted = render <$> (prExpr =<< sParsed)

                xPrinted :: Maybe String
                xPrinted = render <$> (prExpr =<< xRead)


firstWord :: String -> (String,String)
firstWord "" = error "empty test file"
firstWord s = case words s of
    (a:_) -> (a, strip (drop (length a) s))
    _     -> error $ "unknown line format: " ++ s
