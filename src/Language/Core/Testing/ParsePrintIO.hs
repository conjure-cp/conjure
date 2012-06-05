module Language.Core.Testing.ParsePrintIO where

import Language.Core
import Language.Core.Middleware ( runMiddlewareIO, (~>) )
import qualified Language.Core.Middleware.ParseSpec       as ParseSpec       ( worker )
import qualified Language.Core.Middleware.AtMostOneSuchThat as AtMostOneSuchThat ( worker )
import qualified Language.Core.Middleware.PrintSpec       as PrintSpec       ( worker )

-- import qualified Data.Text as T
import qualified Data.Text.IO as T

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit


specs :: [FilePath]
-- specs = map ("EssenceCatalog/fromAlan/" ++) (drop 1 catalog)
specs = map ("/Users/ozgurakgun/src/conjure-wd/testsuite/valid/essence/" ++) (drop 1 testsuite)
     ++ map ("/Users/ozgurakgun/src/conjure-wd/EssenceCatalog/fromAlan/" ++) (drop 1 catalog)
    where
        testsuite = 
            [ ""
            , "1.essence"
            , "2.essence"
            , "3.essence"
            , "4.essence"
            , "5.essence"
            , "6.essence"
            , "7.essence"
            , "8.essence"
            , "9.essence"
            , "complex-quan.essence"
            , "double-quan-guarded.essence"
            , "double-quan.essence"
            , "enum1.essence"
            , "enum2.essence"
            , "forall-0.essence"
            , "forall-1.essence"
            , "forall-2.essence"
            , "forall-sum.essence"
            , "has-funcs.essence"
            , "has-set-max.essence"
            , "has-sets.essence"
            , "holey-matrix.essence"
            , "lambda.essence"
            , "letting-dom.essence"
            , "matrix-hist.essence"
            , "matrix-indby-enum.essence"
            , "matrix-of-set.essence"
            , "mmt.essence"
            , "nesting.essence"
            , "relation.essence"
            , "set-card-union.essence"
            , "set-eq.essence"
            , "set-forall.essence"
            , "set-of-sets.essence"
            , "set-of-tuples.essence"
            , "set-union-quantification.essence"
            , "set-union.essence"
            , "simplest.essence"
            , "sss.essence"
            , "tuple-explode.essence"
            , "two-bars.essence"
            , "unnamed-1.essence"
            ]
        catalog =
            [ ""
            , "prob001.essence"
            , "prob002.essence"
            , "prob005.essence"
            , "prob006.essence"
            , "prob007.essence"
            , "prob008.essence"
            , "prob009.essence"
            , "prob010.essence"
            , "prob013.essence"
            , "prob015.essence"
            , "prob016.essence"
            , "prob018.essence"
            , "prob019.essence"
            , "prob022.essence"
            , "prob023.essence"
            , "prob024.essence"
            , "prob025.essence"
            , "prob026.essence"
            , "prob028.essence"
            , "prob030.essence"
            , "prob031.essence"
            , "prob032.essence"
            , "prob033.essence"
            , "prob034.essence"
            , "prob036.essence"
            , "prob037.essence"
            , "prob038.essence"
            , "prob039.essence"
            , "prob041.essence"
            , "prob109.essence"
            , "prob110.essence"
            , "prob115.essence"
            , "prob116.essence"
            , "prob123.essence"
            , "prob128.essence"
            , "prob129.essence"
            , "prob131.essence"
            , "prob132.essence"
            , "prob133.essence"
            ]


tests :: Test.Hspec.Monadic.Spec
tests = describe "parsing and pretty printing" $ do
    forM_ specs $ \ filepath -> do
        it filepath $ do
            content <- T.readFile filepath
            parsed  <- runMiddlewareIO (filepath, content)
                    $ ParseSpec.worker ~> AtMostOneSuchThat.worker
            printed <- runMiddlewareIO parsed
                    $ PrintSpec.worker
            parsed2 <- runMiddlewareIO ("<memory>", stringToText $ show printed)
                    $ ParseSpec.worker ~> AtMostOneSuchThat.worker
            parsed @?= parsed2
