module Test.Language.EssenceParsePrintCommon where


import Control.Applicative
import Data.Maybe
import Test.HUnit ( Test(..), (~:), (~=?), assertBool, test )

import Language.Essence ( Expr(..) )
import Language.EssenceParsers ( pExpr )
import Language.EssencePrinters ( prExpr )
import ParsecUtils ( Parser, eof, parseMaybe )
import PrintUtils ( render )
import Utils ( maybeRead )


pExprEof :: Parser Expr
pExprEof = pExpr <* eof


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
