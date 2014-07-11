module Main where

-- conjure tests
import qualified Conjure.Language.DomainSizeTest ( tests )
import qualified Conjure.RepresentationsTest ( tests )

-- tasty
import Test.Tasty


main :: IO ()
main = defaultMain $ testGroup "conjure tests"
    [ Conjure.Language.DomainSizeTest.tests
    , Conjure.RepresentationsTest.tests
    ]

