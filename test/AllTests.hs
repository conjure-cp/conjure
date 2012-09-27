module Main ( main ) where

import Test.Hspec.Monadic ( hspec )
import qualified Language.E.Testing.ParsePrintSpec as ParsePrintSpec ( tests )
import qualified Language.E.Testing.RuleEngine as RuleEngine ( tests )

main :: IO ()
main = do
    parsePrintSpecTests <- ParsePrintSpec.tests
    hspec $ do
        parsePrintSpecTests
        RuleEngine.tests
