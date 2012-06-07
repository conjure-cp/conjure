module Main ( main ) where

import Test.Hspec.Monadic ( hspecX )
-- import qualified Language.Core.Testing.ParsePrintIO as ParsePrintIO ( tests )
import qualified Language.Core.Testing.RuleEngine as RuleEngine ( tests )

main :: IO ()
main = hspecX $ do
    -- ParsePrintIO.tests
    RuleEngine.tests
