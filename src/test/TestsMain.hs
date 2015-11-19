module Main where

-- conjure tests
import Conjure.Prelude
import qualified Conjure.Language.DomainSizeTest ( tests )
import qualified Conjure.RepresentationsTest ( tests )
import qualified Conjure.ModelAllSolveAll ( tests, QuickOrSlow(..) )
import qualified Conjure.TypeCheckAll ( tests )
import qualified Conjure.ParsePrint ( tests )
import qualified Golden ( tests )

-- tasty
import Test.Tasty ( defaultMainWithIngredients, defaultIngredients, includingOptions, askOption, testGroup )
import Test.Tasty.Options ( OptionDescription(..) )

-- tasty-ant-xml
import Test.Tasty.Runners.AntXML ( antXMLRunner )

-- tasty-rerun
import Test.Tasty.Ingredients.Rerun ( rerunningTests )


main :: IO ()
main = do
    modelAllSolveAllTests <- Conjure.ModelAllSolveAll.tests
    typeCheckAllTests     <- Conjure.TypeCheckAll.tests
    parsePrintTests       <- Conjure.ParsePrint.tests
    let ingredients =
            [ rerunningTests ( antXMLRunner
                             : includingOptions [Option (Proxy :: Proxy Conjure.ModelAllSolveAll.QuickOrSlow)] 
                             : defaultIngredients
                             ) ]
    defaultMainWithIngredients ingredients $ askOption $ \ quickOrSlow ->
        testGroup "conjure"
            [ Conjure.Language.DomainSizeTest.tests
            , Conjure.RepresentationsTest.tests
            , modelAllSolveAllTests quickOrSlow
            , typeCheckAllTests
            , parsePrintTests
            , Golden.tests
            ]
