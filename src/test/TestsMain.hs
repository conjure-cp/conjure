module Main where

-- conjure tests
import Conjure.Prelude
import qualified Conjure.Language.DomainSizeTest ( tests )
import qualified Conjure.RepresentationsTest ( tests )
import qualified Conjure.ModelAllSolveAll ( tests )
import qualified Golden ( tests )

-- tasty
import Test.Tasty

-- tasty-ant-xml
import Test.Tasty.Runners.AntXML

main :: IO ()
main = do
    modelAllSolveAllTests <- Conjure.ModelAllSolveAll.tests
    defaultMainWithIngredients (antXMLRunner : defaultIngredients)
        $ testGroup "conjure"
            [ Conjure.Language.DomainSizeTest.tests
            , Conjure.RepresentationsTest.tests
            , modelAllSolveAllTests
            , Golden.tests
            ]
