module Main ( main ) where

import Test.Hspec.Monadic ( hspec )
import qualified Language.E.Testing.ParsePrintSpec as ParsePrintSpec ( tests )
import qualified Language.E.Testing.TopToBottom as TopToBottom ( tests )


main :: IO ()
main = do
    parsePrintSpecTests <- ParsePrintSpec.tests
    topToBottomTests <- TopToBottom.tests
    hspec $ do
        parsePrintSpecTests
        topToBottomTests

