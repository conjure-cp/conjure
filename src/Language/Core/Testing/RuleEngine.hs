{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Core.Testing.RuleEngine where

import Language.Core
import Language.Core.Pipeline.ToCore ( toCore, readSpec )

import Prelude hiding ( mapM )
import Data.Traversable ( mapM )

import qualified Text.PrettyPrint as Pr

import qualified Test.Hspec.Monadic
import Test.Hspec.Monadic ( describe, it )
import Test.Hspec.HUnit ()
import Test.HUnit ( assertFailure )


testSetIn :: IO ()
testSetIn = void $ loadAndApply
        "testsuite/ruleengine/setIn.essence"
        [ "testsuite/ruleengine/set-in-to-quantified.rule"
        ]

testSetEq1 :: IO ()
testSetEq1 = void $ loadAndApply
        "testsuite/ruleengine/setEq.essence"
        [ "testsuite/ruleengine/set-eq-to-subsets.rule"
        ]

testSetEq2 :: IO ()
testSetEq2 = void $ loadAndApply
        "testsuite/ruleengine/setEq.essence"
        [ "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        ]

testSetEq3 :: IO ()
testSetEq3 = void $ loadAndApply
        "testsuite/ruleengine/setEq.essence"
        [ "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        ]

testSetEq4 :: IO ()
testSetEq4 = void $ loadAndApply
        "testsuite/ruleengine/setEq.essence"
        [ "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        ]


testSetNeq0 :: IO ()
testSetNeq0 = void $ loadAndApply
        "testsuite/ruleengine/setNeq.essence"
        []

testSetNeq1 :: IO ()
testSetNeq1 = void $ loadAndApply
        "testsuite/ruleengine/setNeq.essence"
        [ "testsuite/ruleengine/set-neq-to-eq.rule"
        ]

testSetNeq2 :: IO ()
testSetNeq2 = void $ loadAndApply
        "testsuite/ruleengine/setNeq.essence"
        [ "testsuite/ruleengine/set-neq-to-eq.rule"
        , "testsuite/ruleengine/set-eq-to-subsets.rule"
        ]

testSetNeq3 :: IO ()
testSetNeq3 = void $ loadAndApply
        "testsuite/ruleengine/setNeq.essence"
        [ "testsuite/ruleengine/set-neq-to-eq.rule"
        , "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        ]

testSetNeq4 :: IO ()
testSetNeq4 = void $ loadAndApply
        "testsuite/ruleengine/setNeq.essence"
        [ "testsuite/ruleengine/set-neq-to-eq.rule"
        , "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        ]

testSetNeq5 :: IO ()
testSetNeq5 = void $ loadAndApply
        "testsuite/ruleengine/setNeq.essence"
        [ "testsuite/ruleengine/set-neq-to-eq.rule"
        , "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        ]

testSetNeq6 :: IO ()
testSetNeq6 = void $ loadAndApply
        "testsuite/ruleengine/setNeq.essence"
        [ "testsuite/ruleengine/set-neq-to-eq.rule"
        , "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        , "testsuite/ruleengine/bogus/set-subseteq-to-quantified-100-Plus.rule"
        ]

testSetNeq7 :: IO ()
testSetNeq7 = void $ loadAndApply
        "testsuite/ruleengine/setNeq.essence"
        [ "testsuite/ruleengine/set-neq-to-eq.rule"
        , "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        , "testsuite/ruleengine/bogus/set-subseteq-to-quantified-1000-Plus.rule"
        ]

testSetNeq8 :: IO ()
testSetNeq8 = void $ loadAndApply
        "testsuite/ruleengine/setNeq.essence"
        [ "testsuite/ruleengine/set-neq-to-eq.rule"
        , "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        , "testsuite/ruleengine/bogus/set-subseteq-to-quantified-1000-Plus.rule"
        , "testsuite/ruleengine/bogus/set-subseteq-to-quantified-1000-Times.rule"
        ]

testSetNeq9 :: IO ()
testSetNeq9 = void $ loadAndApply
        "testsuite/ruleengine/setNeq.essence"
        [ "testsuite/ruleengine/set-neq-to-eq.rule"
        , "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        , "testsuite/ruleengine/bogus/set-subseteq-to-quantified-1000-Plus.rule"
        , "testsuite/ruleengine/bogus/set-subseteq-to-quantified-1000-Minus.rule"
        , "testsuite/ruleengine/bogus/set-subseteq-to-quantified-1000-Times.rule"
        , "testsuite/ruleengine/bogus/set-subseteq-to-quantified-1000-Div.rule"
        , "testsuite/ruleengine/bogus/set-subseteq-to-quantified-1000-Mod.rule"
        ]

testSetMax1 :: IO ()
testSetMax1 = void $ loadAndApply
        "testsuite/ruleengine/setMax.essence"
        [ "testsuite/ruleengine/set-max.rule"
        ]

testSetMax2 :: IO ()
testSetMax2 = void $ loadAndApply
        "testsuite/ruleengine/setMax.essence"
        [ "testsuite/ruleengine/set-max.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        ]

testSetMin1 :: IO ()
testSetMin1 = void $ loadAndApply
        "testsuite/ruleengine/setMin.essence"
        [ "testsuite/ruleengine/set-min.rule"
        ]

testSetMin2 :: IO ()
testSetMin2 = void $ loadAndApply
        "testsuite/ruleengine/setMin.essence"
        [ "testsuite/ruleengine/set-min.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        ]

testSetMaxInObj :: IO ()
testSetMaxInObj = void $ loadAndApply
        "testsuite/ruleengine/setMaxInObj.essence"
        [ "testsuite/ruleengine/set-max.rule"
        , "testsuite/ruleengine/set-min.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        ]

testSetIntersect1 :: IO ()
testSetIntersect1 = runInteractively "setIntersect-1"


loadAndApply :: FilePath -> [FilePath] -> IO ()
loadAndApply spec' rules' = do
    spec    <- pairWithContents spec'
    rules   <- mapM pairWithContents rules'
    let
        mresults = runComp def def (toCore spec rules)
        logs     = mconcat [ ls | (_      , _, ls) <- mresults ]
        errors   =         [ x  | (Left  x, _, _ ) <- mresults ]
        results  =         [ x  | (Right x, _, _ ) <- mresults ]
    mapM_ print $ prettyLog logs
    if null errors
        then mapM_ (print . pretty) results
        else error $ show
                   $ prettyErrors "There were errors in at least one branch." errors

buildTests :: [(String, FilePath, [FilePath], [FilePath])] -> Test.Hspec.Monadic.Spec
-- buildTests = undefined
buildTests params = describe "rule engine" $ do
    forM_ params $ \ (name, spec', outputs', rules') -> do
        it ("testing " ++ name) $ do

            spec    <- pairWithContents spec'
            rules   <- mapM pairWithContents rules'
            outputs <- mapM pairWithContents outputs'

            let
                mgenerateds = runComp def def (toCore spec rules)
                logsG       = mconcat [ ls | (_      , _, ls) <- mgenerateds ]
                errorsG     =         [ x  | (Left  x, _, _ ) <- mgenerateds ]
                generateds  =         [ x  | (Right x, _, _ ) <- mgenerateds ]
            mapM_ print $ prettyLog logsG
            unless (null errorsG)
                $ assertFailure
                $ show
                $ prettyErrors "There were errors in at least one branch." errorsG

            let
                mexpecteds = runComp def def (mapM readSpec outputs)
                logsE      = mconcat [ ls | (_        , _, ls) <- mexpecteds ]
                errorsE    =         [ x  | (Left  x  , _, _ ) <- mexpecteds ]
                expecteds  =  concat [ xs  | (Right xs, _, _ ) <- mexpecteds ]
            mapM_ print $ prettyLog logsE
            unless (null errorsE)
                $ assertFailure
                $ show
                $ prettyErrors "There were errors in at least one branch." errorsE

            ppPrint ( length outputs'
                    , length expecteds
                    , length generateds
                    )
            unless (length generateds == length expecteds)
                $ assertFailure
                $ show
                $ vcat [ "different number of outputs generated."
                       , "expected:"  <++> (stringToDoc $ show $ length expecteds )
                       , "generated:" <++> (stringToDoc $ show $ length generateds)
                       ]

            forM_ (zip3 [(1::Int) ..] generateds expecteds) $ \ (i,generated,expected) ->
                unless (generated == expected)
                    $ assertFailure
                    $ Pr.renderStyle Pr.style { Pr.lineLength = 120 }
                        $ "specs not equal"
                            <+> Pr.parens (stringToDoc $ show i)
                                $$ vcat [ " == generated ==" $$ pretty generated
                                        , " == expected  ==" $$ pretty expected
                                        ]


runInteractively :: String -> IO ()
runInteractively name = case [ (input,rules) | (name',input,_,rules) <- testData, name == name' ] of
    [(input,rules)] -> void $ loadAndApply input rules
    _               -> error $ "not found " ++ name

tests :: Test.Hspec.Monadic.Spec
tests = buildTests testData

testData :: [ ( String              -- a name for the test case
              , FilePath            -- input Essence spec
              , [FilePath]          -- output specs
              , [FilePath]          -- rules to use
              ) ]
testData =
    [ (   "setIn"
      ,   "testsuite/ruleengine/setIn.essence"
      , [ "testsuite/ruleengine/out/testSetIn.essence"
        ]
      , [ "testsuite/ruleengine/set-in-to-quantified.rule"
        ]
      )



    , (   "setEq-0"
      ,   "testsuite/ruleengine/setEq.essence"
      , []
      , []
      )

    , (   "setEq-1"
      ,   "testsuite/ruleengine/setEq.essence"
      , [ "testsuite/ruleengine/out/testSetEq1.essence"
        ]
      , [ "testsuite/ruleengine/set-eq-to-subsets.rule"
        ]
      )

    , (   "setEq-2"
      ,   "testsuite/ruleengine/setEq.essence"
      , [ "testsuite/ruleengine/out/testSetEq2.essence"
        ]
      , [ "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        ]
      )

    , (   "setEq-3"
      ,   "testsuite/ruleengine/setEq.essence"
      , [ "testsuite/ruleengine/out/testSetEq3.essence"
        ]
      , [ "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        ]
      )

    , (   "setEq-4"
      ,   "testsuite/ruleengine/setEq.essence"
      , [ "testsuite/ruleengine/out/testSetEq4.essence"
        ]
      , [ "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        ]
      )



    , (   "setLiteralEq-1"
      ,   "testsuite/ruleengine/setLiteralEq.essence"
      , [ "testsuite/ruleengine/out/testSetLiteralEq1.essence"
        ]
      , allrules
      )



    , (   "setNeq-0"
      ,   "testsuite/ruleengine/setNeq.essence"
      , []
      , []
      )

    , (   "setNeq-1"
      ,   "testsuite/ruleengine/setNeq.essence"
      , [ "testsuite/ruleengine/out/testSetNeq1.essence"
        ]
      , [ "testsuite/ruleengine/set-neq-to-eq.rule"
        ]
      )

    , (   "setNeq-2"
      ,   "testsuite/ruleengine/setNeq.essence"
      , [ "testsuite/ruleengine/out/testSetNeq2.essence"
        ]
      , [ "testsuite/ruleengine/set-neq-to-eq.rule"
        , "testsuite/ruleengine/set-eq-to-subsets.rule"
        ]
      )

    , (   "setNeq-3"
      ,   "testsuite/ruleengine/setNeq.essence"
      , [ "testsuite/ruleengine/out/testSetNeq3.essence"
        ]
      , [ "testsuite/ruleengine/set-neq-to-eq.rule"
        , "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        ]
      )

    , (   "setNeq-4"
      ,   "testsuite/ruleengine/setNeq.essence"
      , [ "testsuite/ruleengine/out/testSetNeq4.essence"
        ]
      , [ "testsuite/ruleengine/set-neq-to-eq.rule"
        , "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        ]
      )

    , (   "setNeq-5"
      ,   "testsuite/ruleengine/setNeq.essence"
      , [ "testsuite/ruleengine/out/testSetNeq5.essence"
        ]
      , [ "testsuite/ruleengine/set-neq-to-eq.rule"
        , "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        ]
      )

    , (   "setNeq-6"
      ,   "testsuite/ruleengine/setNeq.essence"
      , [ "testsuite/ruleengine/out/testSetNeq6.essence"
        ]
      , [ "testsuite/ruleengine/set-neq-to-eq.rule"
        , "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        , "testsuite/ruleengine/bogus/set-subseteq-to-quantified-100-Plus.rule"
        ]
      )

    , (   "setNeq-7"
      ,   "testsuite/ruleengine/setNeq.essence"
      , [ "testsuite/ruleengine/out/testSetNeq7-" ++ show i ++ ".essence"
        | i <- [ (1::Int) .. 4 ]
        ]
      , [ "testsuite/ruleengine/set-neq-to-eq.rule"
        , "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        , "testsuite/ruleengine/bogus/set-subseteq-to-quantified-1000-Plus.rule"
        ]
      )

    , (   "setNeq-8"
      ,   "testsuite/ruleengine/setNeq.essence"
      , [ "testsuite/ruleengine/out/testSetNeq8-" ++ show i ++ ".essence"
        | i <- [ (1::Int) .. 9 ]
        ]
      , [ "testsuite/ruleengine/set-neq-to-eq.rule"
        , "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        , "testsuite/ruleengine/bogus/set-subseteq-to-quantified-1000-Plus.rule"
        , "testsuite/ruleengine/bogus/set-subseteq-to-quantified-1000-Times.rule"
        ]
      )

    , (   "setNeq-9"
      ,   "testsuite/ruleengine/setNeq.essence"
      , [ "testsuite/ruleengine/out/testSetNeq9-" ++ show i ++ ".essence"
        | i <- [ (1::Int) .. 36 ]
        ]
      , [ "testsuite/ruleengine/set-neq-to-eq.rule"
        , "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        , "testsuite/ruleengine/bogus/set-subseteq-to-quantified-1000-Plus.rule"
        , "testsuite/ruleengine/bogus/set-subseteq-to-quantified-1000-Minus.rule"
        , "testsuite/ruleengine/bogus/set-subseteq-to-quantified-1000-Times.rule"
        , "testsuite/ruleengine/bogus/set-subseteq-to-quantified-1000-Div.rule"
        , "testsuite/ruleengine/bogus/set-subseteq-to-quantified-1000-Mod.rule"
        ]
      )


    , (   "setMax-1"
      ,   "testsuite/ruleengine/setMax.essence"
      , [ "testsuite/ruleengine/out/testSetMax1.essence"
        ]
      , [ "testsuite/ruleengine/set-max.rule"
        ]
      )

    , (   "setMax-2"
      ,   "testsuite/ruleengine/setMax.essence"
      , [ "testsuite/ruleengine/out/testSetMax2.essence"
        ]
      , [ "testsuite/ruleengine/set-max.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        ]
      )


    , (   "setMin-1"
      ,   "testsuite/ruleengine/setMin.essence"
      , [ "testsuite/ruleengine/out/testSetMin1.essence"
        ]
      , [ "testsuite/ruleengine/set-min.rule"
        ]
      )

    , (   "setMin-2"
      ,   "testsuite/ruleengine/setMin.essence"
      , [ "testsuite/ruleengine/out/testSetMin2.essence"
        ]
      , [ "testsuite/ruleengine/set-min.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        ]
      )


    , (   "setMaxInObj"
      ,   "testsuite/ruleengine/setMaxInObj.essence"
      , [ "testsuite/ruleengine/out/testSetMaxInObj.essence"
        ]
      , [ "testsuite/ruleengine/set-max.rule"
        , "testsuite/ruleengine/set-min.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        ]
      )


    , ( "setIntersect-1"
      , "testsuite/ruleengine/setIntersect.essence"
      , [ "testsuite/ruleengine/out/testSetIntersect1.essence"
        ]
      , [ "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        , "testsuite/ruleengine/set-intersect-quantifier.rule"
        ]
      )

    , ( "setUnion-1"
      , "testsuite/ruleengine/setUnion.essence"
      , [ "testsuite/ruleengine/out/testSetUnion1.essence"
        ]
      , [ "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        , "testsuite/ruleengine/set-intersect-quantifier.rule"
        , "testsuite/ruleengine/set-union-forAll.rule"
        , "testsuite/ruleengine/set-union-exists.rule"
        ]
      )

    , ( "setMinus-1"
      , "testsuite/ruleengine/setMinus.essence"
      , [ "testsuite/ruleengine/out/testSetMinus1.essence"
        ]
      , [ "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        , "testsuite/ruleengine/set-minus-quantifier.rule"
        ]
      )

    , ( "setCard-1"
      , "testsuite/ruleengine/setCard.essence"
      , [ "testsuite/ruleengine/out/testSetCard1.essence"
        ]
      , [ "testsuite/ruleengine/set-card.rule"
        , "testsuite/ruleengine/set-in-to-quantified.rule"
        ]
      )

    , ( "setToMSet-1"
      , "testsuite/ruleengine/setToMSet.essence"
      , [ "testsuite/ruleengine/out/testSetToMSet1.essence"
        ]
      , [ "testsuite/ruleengine/set-toMSet-to-quantified.rule"
        ]
      )



    , ( "msetIn-0"
      , "testsuite/ruleengine/msetIn.essence"
      , [ ]
      , [ "testsuite/ruleengine/set-in-to-quantified.rule"
        ]
      )

    , ( "msetIn-1"
      , "testsuite/ruleengine/msetIn.essence"
      , [ "testsuite/ruleengine/out/testMSetIn1.essence"
        ]
      , [ "testsuite/ruleengine/mset-in-to-freq.rule"
        ]
      )

    , ( "msetEq-0"
      , "testsuite/ruleengine/msetEq.essence"
      , [ ]
      , [ "testsuite/ruleengine/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/set-supseteq-to-subseteq.rule"
        ]
      )

    , ( "msetEq-1"
      , "testsuite/ruleengine/msetEq.essence"
      , [ "testsuite/ruleengine/out/testMSetEq1.essence"
        ]
      , [ "testsuite/ruleengine/mset-eq-to-subsets.rule"
        , "testsuite/ruleengine/mset-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/mset-subseteq-to-quantified.rule"
        ]
      )

    , ( "msetCard-1"
      , "testsuite/ruleengine/msetCard.essence"
      , [ "testsuite/ruleengine/out/testMSetCard1.essence"
        ]
      , [ "testsuite/ruleengine/mset-card.rule"
        , "testsuite/ruleengine/mset-in-to-freq.rule"
        ]
      )


    , ( "msetIntersect1-1"
      , "testsuite/ruleengine/msetIntersect1.essence"
      , [ "testsuite/ruleengine/out/testMSetIntersect1-1.essence"
        ]
      , [ "testsuite/ruleengine/mset-eq-to-subsets.rule"
        , "testsuite/ruleengine/mset-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/mset-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/mset-in-to-freq.rule"
        , "testsuite/ruleengine/mset-intersect-forAll.rule"
        , "testsuite/ruleengine/mset-intersect-exists.rule"
        , "testsuite/ruleengine/mset-intersect-sum.rule"
        ]
      )

    , ( "msetIntersect1-2"
      , "testsuite/ruleengine/msetIntersect1.essence"
      , [ "testsuite/ruleengine/out/testMSetIntersect1-2.essence"
        ]
      , [ "testsuite/ruleengine/mset-eq-to-subsets.rule"
        , "testsuite/ruleengine/mset-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/mset-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/mset-in-to-freq.rule"
        , "testsuite/ruleengine/mset-intersect-forAll.rule"
        , "testsuite/ruleengine/mset-intersect-exists.rule"
        , "testsuite/ruleengine/mset-intersect-sum.rule"
        , "testsuite/ruleengine/mset-freq-to-sum.rule"
        ]
      )

    , ( "msetIntersect2-1"
      , "testsuite/ruleengine/msetIntersect2.essence"
      , [ "testsuite/ruleengine/out/testMSetIntersect2-1.essence"
        ]
      , [ "testsuite/ruleengine/mset-eq-to-subsets.rule"
        , "testsuite/ruleengine/mset-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/mset-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/mset-in-to-freq.rule"
        , "testsuite/ruleengine/mset-intersect-forAll.rule"
        , "testsuite/ruleengine/mset-intersect-exists.rule"
        , "testsuite/ruleengine/mset-intersect-sum.rule"
        , "testsuite/ruleengine/mset-card.rule"
        ]
      )

    , ( "msetIntersect2-2"
      , "testsuite/ruleengine/msetIntersect2.essence"
      , [ "testsuite/ruleengine/out/testMSetIntersect2-2.essence"
        ]
      , [ "testsuite/ruleengine/mset-eq-to-subsets.rule"
        , "testsuite/ruleengine/mset-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/mset-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/mset-in-to-freq.rule"
        , "testsuite/ruleengine/mset-intersect-forAll.rule"
        , "testsuite/ruleengine/mset-intersect-exists.rule"
        , "testsuite/ruleengine/mset-intersect-sum.rule"
        , "testsuite/ruleengine/mset-card.rule"
        , "testsuite/ruleengine/mset-freq-to-sum.rule"
        ]
      )

    , ( "msetIntersect3-1"
      , "testsuite/ruleengine/msetIntersect3.essence"
      , [ "testsuite/ruleengine/out/testMSetIntersect3-1.essence"
        ]
      , [ "testsuite/ruleengine/mset-eq-to-subsets.rule"
        , "testsuite/ruleengine/mset-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/mset-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/mset-in-to-freq.rule"
        , "testsuite/ruleengine/mset-intersect-forAll.rule"
        , "testsuite/ruleengine/mset-intersect-exists.rule"
        , "testsuite/ruleengine/mset-intersect-sum.rule"
        ]
      )

    , ( "msetIntersect3-2"
      , "testsuite/ruleengine/msetIntersect3.essence"
      , [ "testsuite/ruleengine/out/testMSetIntersect3-2.essence"
        ]
      , [ "testsuite/ruleengine/mset-eq-to-subsets.rule"
        , "testsuite/ruleengine/mset-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/mset-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/mset-in-to-freq.rule"
        , "testsuite/ruleengine/mset-intersect-forAll.rule"
        , "testsuite/ruleengine/mset-intersect-exists.rule"
        , "testsuite/ruleengine/mset-intersect-sum.rule"
        , "testsuite/ruleengine/mset-freq-to-sum.rule"
        ]
      )

    , ( "sets-chris1"
      , "testsuite/ruleengine/sets-chris1.essence"
      , [ "testsuite/ruleengine/sets-chris1.expected.essence" ]
      , allrules
      )

    , ( "sets-chris2"
      , "testsuite/ruleengine/sets-chris2.essence"
      , [ "testsuite/ruleengine/sets-chris2.expected.essence" ]
      , allrules
      )

    ]



allrules :: [FilePath]
allrules = map ("testsuite/ruleengine/"++)
            [ "mset-card.rule"
            , "mset-eq-to-subsets.rule"
            , "mset-freq-to-sum.rule"
            , "mset-in-to-freq.rule"
            , "mset-intersect-exists.rule"
            , "mset-intersect-forAll.rule"
            , "mset-intersect-sum.rule"
            , "mset-subset-to-subsetEq.rule"
            , "mset-subseteq-to-quantified.rule"
            , "mset-supset-to-subset.rule"
            , "mset-supseteq-to-subseteq.rule"
            , "mset-toset-exists.rule"
            , "mset-toset-forAll.rule"
            , "mset-toset-sum.rule"
            , "mset-union-exists.rule"
            , "mset-union-forAll.rule"
            , "mset-union-sum.rule"
            , "set-card.rule"
            , "set-eq-to-subsets.rule"
            , "set-in-to-quantified.rule"
            , "set-intersect-quantifier.rule"
            , "set-max.rule"
            , "set-min.rule"
            , "set-minus-quantifier.rule"
            , "set-neq-to-eq.rule"
            , "set-subset-to-subsetEq.rule"
            , "set-subseteq-to-quantified.rule"
            , "set-supset-to-subset.rule"
            , "set-supseteq-to-subseteq.rule"
            , "set-toMSet-to-quantified.rule"
            , "set-union-exists.rule"
            , "set-union-forAll.rule"
            , "set-union-sum.rule"
            ]
