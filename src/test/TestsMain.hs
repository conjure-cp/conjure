module Main where

-- conjure tests
import Conjure.Prelude
import qualified Conjure.Language.DomainSizeTest ( tests )
import qualified Conjure.RepresentationsTest ( tests )
import qualified Conjure.ModelAllSolveAll ( tests )

-- tasty
import Test.Tasty


main :: IO ()
main = do
    modelAllSolveAllTests <- Conjure.ModelAllSolveAll.tests
    defaultMain $ testGroup "conjure"
        [ Conjure.Language.DomainSizeTest.tests
        , Conjure.RepresentationsTest.tests
        , modelAllSolveAllTests
        ]

