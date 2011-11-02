module Test.Language.EssenceParsePrintCommon where


import Control.Applicative
import Data.Maybe
import Test.HUnit ( Test(..), (~:), (~=?), assertBool, test )

import Language.Essence ( Expr(..), Log )
import Language.EssenceParsers ( pExpr )
import Language.EssencePrinters ( prExpr )
import Language.EssenceEvaluator ( runEvaluateExpr )
import ParsecUtils ( Parser, eof, parseMaybe )
import PrintUtils ( render )
import Utils ( maybeRead )


pExprEof :: Parser Expr
pExprEof = pExpr <* eof


cmdShouldParse :: String -> Test
cmdShouldParse s = test [ TestCase $ assertBool "ShouldParse" $ isJust sParsed
                        ]
    where
        sParsed :: Maybe Expr
        sParsed = parseMaybe pExprEof s


cmdNoParse :: String -> Test
cmdNoParse s = test [ TestCase $ assertBool "NoParse" $ isNothing sParsed
                    ]
    where
        sParsed :: Maybe Expr
        sParsed = parseMaybe pExprEof s


cmdShouldParseTo :: String -> String -> Test
cmdShouldParseTo s x = test [ TestCase $ assertBool ("ShouldParseTo.parse: " ++ s) $ isJust sParsed
                            , TestCase $ assertBool ("ShouldParseTo.read: "  ++ x) $ isJust xRead
                            , "ShouldParseTo.(s.parsed=x.read)" ~: sParsed ~=? xRead
                            ]
    where
        sParsed :: Maybe Expr
        sParsed = parseMaybe pExprEof s

        xRead :: Maybe Expr
        xRead = maybeRead x


cmdParsePrint :: String -> String -> Test
cmdParsePrint s x = test [ TestCase $ assertBool ("ParsePrint.parse: " ++ s) $ isJust sParsed
                         , TestCase $ assertBool ("ParsePrint.read: "  ++ x) $ isJust xRead
                         , TestCase $ assertBool ("ParsePrint.parsePrint" ++ s) $ isJust sParsedPrinted
                         , TestCase $ assertBool ("ParsePrint.readPrint"  ++ x) $ isJust xPrinted
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


cmdEval :: String -> String -> Test
cmdEval i j = test [ TestCase $ assertBool ("Eval.parse: " ++ i) $ isJust iParsed
                   , TestCase $ assertBool ("Eval.parse: " ++ j) $ isJust jParsed
                   , TestCase $ assertBool ("Eval.parsePrint: " ++ i) $ isJust iEvalPrinted
                   , TestCase $ assertBool ("Eval.parsePrint: " ++ j) $ isJust jParsedPrinted
                   , ("Eval" ++ unlines logs) ~: jParsedPrinted ~=? iEvalPrinted
                   ]
    where
        iParsed :: Maybe Expr
        iParsed = parseMaybe pExprEof i

        iEvalWithLogs :: Maybe (Expr,[Log])
        iEvalWithLogs = do
            t <- iParsed
            return $ runEvaluateExpr [] t

        iEval :: Maybe Expr
        iEval = fmap fst iEvalWithLogs

        logs :: [Log]
        logs = fromMaybe [] $ fmap snd iEvalWithLogs

        iEvalPrinted :: Maybe String
        iEvalPrinted = render <$> (prExpr =<< iEval)

        jParsed :: Maybe Expr
        jParsed = parseMaybe pExprEof j

        jParsedPrinted :: Maybe String
        jParsedPrinted = render <$> (prExpr =<< jParsed)
