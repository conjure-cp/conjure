module Main where

-- conjure tests
import qualified Conjure.Language.DomainSizeSpec ( tests )
import qualified Conjure.RepresentationsSpec ( tests )

-- tasty
import Test.Tasty


main :: IO ()
main = defaultMain $ testGroup "conjure tests"
    [ Conjure.Language.DomainSizeSpec.tests
    , Conjure.RepresentationsSpec.tests
    ]

