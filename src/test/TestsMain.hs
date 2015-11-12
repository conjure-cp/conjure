module Main where

-- conjure tests
import Conjure.Prelude
import qualified Conjure.Language.DomainSizeTest ( tests )
import qualified Conjure.RepresentationsTest ( tests )
import qualified Conjure.ModelAllSolveAll ( tests )
import qualified Conjure.TypeCheckAll ( tests )
import qualified Conjure.ParsePrint ( tests )
import qualified Golden ( tests )

-- tasty
import Test.Tasty ( defaultMainWithIngredients, defaultIngredients, testGroup )

-- tasty-ant-xml
import Test.Tasty.Runners.AntXML ( antXMLRunner )

-- tasty-rerun
import Test.Tasty.Ingredients.Rerun ( rerunningTests )


main :: IO ()
main = do
    modelAllSolveAllTests <- Conjure.ModelAllSolveAll.tests
    typeCheckAllTests <- Conjure.TypeCheckAll.tests
    parsePrintTests <- Conjure.ParsePrint.tests
    defaultMainWithIngredients [rerunningTests (antXMLRunner : defaultIngredients)]
        $ testGroup "conjure"
            [ Conjure.Language.DomainSizeTest.tests
            , Conjure.RepresentationsTest.tests
            , modelAllSolveAllTests
            , typeCheckAllTests
            , parsePrintTests
            , Golden.tests
            ]
