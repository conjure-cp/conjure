{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.E.Testing.RuleEngine where

import Language.E
import Language.E.Pipeline.ToCore ( toCore, readSpec )

import Prelude hiding ( mapM )
import Data.Traversable ( mapM )

import qualified Text.PrettyPrint as Pr

import qualified Test.Hspec.Monadic
import Test.Hspec.Monadic ( describe, it )
import Test.Hspec.HUnit ()
import Test.HUnit ( assertFailure )


testSetIn :: IO ()
testSetIn = void $ loadAndApply
        "testsuite/ruleengine/specs/setIn.essence"
        [ "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        ]

testSetEq1 :: IO ()
testSetEq1 = void $ loadAndApply
        "testsuite/ruleengine/specs/setEq.essence"
        [ "testsuite/ruleengine/rules/set-eq.rule"
        ]

testSetEq2 :: IO ()
testSetEq2 = void $ loadAndApply
        "testsuite/ruleengine/specs/setEq.essence"
        [ "testsuite/ruleengine/rules/set-eq.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        ]

testSetEq3 :: IO ()
testSetEq3 = void $ loadAndApply
        "testsuite/ruleengine/specs/setEq.essence"
        [ "testsuite/ruleengine/rules/set-eq.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        ]

testSetEq4 :: IO ()
testSetEq4 = void $ loadAndApply
        "testsuite/ruleengine/specs/setEq.essence"
        [ "testsuite/ruleengine/rules/set-eq.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        ]


testSetNeq0 :: IO ()
testSetNeq0 = void $ loadAndApply
        "testsuite/ruleengine/specs/setNeq.essence"
        []

testSetNeq1 :: IO ()
testSetNeq1 = void $ loadAndApply
        "testsuite/ruleengine/specs/setNeq.essence"
        [ "testsuite/ruleengine/rules/set-neq-to-eq.rule"
        ]

testSetNeq2 :: IO ()
testSetNeq2 = void $ loadAndApply
        "testsuite/ruleengine/specs/setNeq.essence"
        [ "testsuite/ruleengine/rules/set-neq-to-eq.rule"
        , "testsuite/ruleengine/rules/set-eq.rule"
        ]

testSetNeq3 :: IO ()
testSetNeq3 = void $ loadAndApply
        "testsuite/ruleengine/specs/setNeq.essence"
        [ "testsuite/ruleengine/rules/set-neq-to-eq.rule"
        , "testsuite/ruleengine/rules/set-eq.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        ]

testSetNeq4 :: IO ()
testSetNeq4 = void $ loadAndApply
        "testsuite/ruleengine/specs/setNeq.essence"
        [ "testsuite/ruleengine/rules/set-neq-to-eq.rule"
        , "testsuite/ruleengine/rules/set-eq.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        ]

testSetNeq5 :: IO ()
testSetNeq5 = void $ loadAndApply
        "testsuite/ruleengine/specs/setNeq.essence"
        [ "testsuite/ruleengine/rules/set-neq-to-eq.rule"
        , "testsuite/ruleengine/rules/set-eq.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        ]

testSetNeq6 :: IO ()
testSetNeq6 = void $ loadAndApply
        "testsuite/ruleengine/specs/setNeq.essence"
        [ "testsuite/ruleengine/rules/set-neq-to-eq.rule"
        , "testsuite/ruleengine/rules/set-eq.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        , "testsuite/ruleengine/rules/bogus/set-subseteq-to-quantified-100-Plus.rule"
        ]

testSetNeq7 :: IO ()
testSetNeq7 = void $ loadAndApply
        "testsuite/ruleengine/specs/setNeq.essence"
        [ "testsuite/ruleengine/rules/set-neq-to-eq.rule"
        , "testsuite/ruleengine/rules/set-eq.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        , "testsuite/ruleengine/rules/bogus/set-subseteq-to-quantified-1000-Plus.rule"
        ]

testSetNeq8 :: IO ()
testSetNeq8 = void $ loadAndApply
        "testsuite/ruleengine/specs/setNeq.essence"
        [ "testsuite/ruleengine/rules/set-neq-to-eq.rule"
        , "testsuite/ruleengine/rules/set-eq.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        , "testsuite/ruleengine/rules/bogus/set-subseteq-to-quantified-1000-Plus.rule"
        , "testsuite/ruleengine/rules/bogus/set-subseteq-to-quantified-1000-Times.rule"
        ]

testSetNeq9 :: IO ()
testSetNeq9 = void $ loadAndApply
        "testsuite/ruleengine/specs/setNeq.essence"
        [ "testsuite/ruleengine/rules/set-neq-to-eq.rule"
        , "testsuite/ruleengine/rules/set-eq.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        , "testsuite/ruleengine/rules/bogus/set-subseteq-to-quantified-1000-Plus.rule"
        , "testsuite/ruleengine/rules/bogus/set-subseteq-to-quantified-1000-Minus.rule"
        , "testsuite/ruleengine/rules/bogus/set-subseteq-to-quantified-1000-Times.rule"
        , "testsuite/ruleengine/rules/bogus/set-subseteq-to-quantified-1000-Div.rule"
        , "testsuite/ruleengine/rules/bogus/set-subseteq-to-quantified-1000-Mod.rule"
        ]

testSetMax1 :: IO ()
testSetMax1 = void $ loadAndApply
        "testsuite/ruleengine/specs/setMax.essence"
        [ "testsuite/ruleengine/rules/set-max.rule"
        ]

testSetMax2 :: IO ()
testSetMax2 = void $ loadAndApply
        "testsuite/ruleengine/specs/setMax.essence"
        [ "testsuite/ruleengine/rules/set-max.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        ]

testSetMin1 :: IO ()
testSetMin1 = void $ loadAndApply
        "testsuite/ruleengine/specs/setMin.essence"
        [ "testsuite/ruleengine/rules/set-min.rule"
        ]

testSetMin2 :: IO ()
testSetMin2 = void $ loadAndApply
        "testsuite/ruleengine/specs/setMin.essence"
        [ "testsuite/ruleengine/rules/set-min.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        ]

testSetMaxInObj :: IO ()
testSetMaxInObj = void $ loadAndApply
        "testsuite/ruleengine/specs/setMaxInObj.essence"
        [ "testsuite/ruleengine/rules/set-max.rule"
        , "testsuite/ruleengine/rules/set-min.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        ]

testSetIntersect1 :: IO ()
testSetIntersect1 = runInteractively "setIntersect-1"


loadAndApply :: FilePath -> [FilePath] -> IO ()
loadAndApply spec' rules' = do
    spec    <- pairWithContents spec'
    rules   <- mapM pairWithContents rules'
    let
        mresults = runIdentity $ runCompE (toCore spec rules)
        logs     = mconcat [ ls | (_      , _, ls) <- mresults ]
        errors   =         [ x  | (Left  x, _, _ ) <- mresults ]
        results  =         [ x  | (Right x, _, _ ) <- mresults ]
    print $ prettyLogs logs
    if null errors
        then
            if null results
                then error "null results"
                else mapM_ (print . pretty) results
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
                mgenerateds = runIdentity $ runCompE (toCore spec rules)
                logsG       = mconcat [ ls | (_      , _, ls) <- mgenerateds ]
                errorsG     =         [ x  | (Left  x, _, _ ) <- mgenerateds ]
                generateds  =         [ x  | (Right x, _, _ ) <- mgenerateds ]
            print $ prettyLogs logsG
            unless (null errorsG)
                $ assertFailure
                $ show
                $ prettyErrors "There were errors in at least one branch." errorsG

            let
                mexpecteds = runIdentity $ runCompE (mapM readSpec outputs)
                logsE      = mconcat [ ls | (_        , _, ls) <- mexpecteds ]
                errorsE    =         [ x  | (Left  x  , _, _ ) <- mexpecteds ]
                expecteds  =  concat [ xs  | (Right xs, _, _ ) <- mexpecteds ]
            print $ prettyLogs logsE
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

            -- forM_ (zip generateds expecteds) $ \ (Spec _ generated,Spec _ expected) ->
            --     if length generated /= length expected
            --         then assertFailure "different number of children."
            --         else
            --             let
            --                 diffs = [ (g,e)
            --                         | (g,e) <- zip generated expected
            --                         , g /= e
            --                         ]
            --             in  if null diffs
            --                     then return ()
            --                     else assertFailure
            --                             $ Pr.renderStyle Pr.style {Pr.lineLength=120}
            --                             $ vcat $ concat [ [ " == generated ==" $$ prettyAsPaths g
            --                                               , " == expected  ==" $$ prettyAsPaths e
            --                                               ]
            --                                             | (g,e) <- diffs
            --                                             ]

            forM_ (zip3 [(1::Int) ..] generateds expecteds) $ \ (i,generated,expected) ->
                unless (generated == expected)
                    $ assertFailure
                    $ Pr.renderStyle Pr.style { Pr.lineLength = 120 }
                        $ "specs not equal"
                            <+> Pr.parens (stringToDoc $ show i)
                                -- $$ vcat [ " == generated ==" $$ prettyAsPaths (generated :: Spec)
                                --         , " == expected  ==" $$ prettyAsPaths expected
                                --         ]
                                -- $$ vcat [ " == generated ==" $$ (stringToDoc $ show generated)
                                --         , " == expected  ==" $$ (stringToDoc $ show expected)
                                --         ]
                                $$ vcat [ " == generated ==" $$ pretty generated
                                        , " == expected  ==" $$ pretty expected
                                        ]


runInteractively :: String -> IO ()
runInteractively name = case [ (input,rules) | (name',input,_,rules) <- testData, name == name' ] of
    [(input,rules)] -> void $ loadAndApply input rules
    _               -> error $ "not found " ++ name

tests :: Test.Hspec.Monadic.Spec
tests = buildTests $ take 25 $ testData

testData :: [ ( String              -- a name for the test case
              , FilePath            -- input Essence spec
              , [FilePath]          -- output specs
              , [FilePath]          -- rules to use
              ) ]
testData =
    [ (   "setIn"
      ,   "testsuite/ruleengine/specs/setIn.essence"
      , [ "testsuite/ruleengine/specs/out/testSetIn.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        ]
      )



    , (   "setEq-0"
      ,   "testsuite/ruleengine/specs/setEq.essence"
      , []
      , []
      )

    , (   "setEq-1"
      ,   "testsuite/ruleengine/specs/setEq.essence"
      , [ "testsuite/ruleengine/specs/out/testSetEq1.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-eq-to-subsets.rule"
        ]
      )

    , (   "setEq-2"
      ,   "testsuite/ruleengine/specs/setEq.essence"
      , [ "testsuite/ruleengine/specs/out/testSetEq2.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        ]
      )

    , (   "setEq-3"
      ,   "testsuite/ruleengine/specs/setEq.essence"
      , [ "testsuite/ruleengine/specs/out/testSetEq3.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        ]
      )

    , (   "setEq-4"
      ,   "testsuite/ruleengine/specs/setEq.essence"
      , [ "testsuite/ruleengine/specs/out/testSetEq4.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        ]
      )



    , (   "setLiteralEq-1"
      ,   "testsuite/ruleengine/specs/setLiteralEq.essence"
      , [ "testsuite/ruleengine/specs/out/testSetLiteralEq1.essence"
        ]
      , allrules
      )



    , (   "setNeq-0"
      ,   "testsuite/ruleengine/specs/setNeq.essence"
      , []
      , []
      )

    , (   "setNeq-1"
      ,   "testsuite/ruleengine/specs/setNeq.essence"
      , [ "testsuite/ruleengine/specs/out/testSetNeq1.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-neq-to-eq.rule"
        ]
      )

    , (   "setNeq-2"
      ,   "testsuite/ruleengine/specs/setNeq.essence"
      , [ "testsuite/ruleengine/specs/out/testSetNeq2.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-neq-to-eq.rule"
        , "testsuite/ruleengine/rules/set-eq-to-subsets.rule"
        ]
      )

    , (   "setNeq-3"
      ,   "testsuite/ruleengine/specs/setNeq.essence"
      , [ "testsuite/ruleengine/specs/out/testSetNeq3.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-neq-to-eq.rule"
        , "testsuite/ruleengine/rules/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        ]
      )

    , (   "setNeq-4"
      ,   "testsuite/ruleengine/specs/setNeq.essence"
      , [ "testsuite/ruleengine/specs/out/testSetNeq4.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-neq-to-eq.rule"
        , "testsuite/ruleengine/rules/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        ]
      )

    , (   "setNeq-5"
      ,   "testsuite/ruleengine/specs/setNeq.essence"
      , [ "testsuite/ruleengine/specs/out/testSetNeq5.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-neq-to-eq.rule"
        , "testsuite/ruleengine/rules/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        ]
      )

    , (   "setNeq-6"
      ,   "testsuite/ruleengine/specs/setNeq.essence"
      , [ "testsuite/ruleengine/specs/out/testSetNeq6.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-neq-to-eq.rule"
        , "testsuite/ruleengine/rules/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        , "testsuite/ruleengine/rules/bogus/set-subseteq-to-quantified-100-Plus.rule"
        ]
      )

    , (   "setNeq-7"
      ,   "testsuite/ruleengine/specs/setNeq.essence"
      , [ "testsuite/ruleengine/specs/out/testSetNeq7-" ++ show i ++ ".essence"
        | i <- [ (1::Int) .. 4 ]
        ]
      , [ "testsuite/ruleengine/rules/set-neq-to-eq.rule"
        , "testsuite/ruleengine/rules/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        , "testsuite/ruleengine/rules/bogus/set-subseteq-to-quantified-1000-Plus.rule"
        ]
      )

    , (   "setNeq-8"
      ,   "testsuite/ruleengine/specs/setNeq.essence"
      , [ "testsuite/ruleengine/specs/out/testSetNeq8-" ++ show i ++ ".essence"
        | i <- [ (1::Int) .. 9 ]
        ]
      , [ "testsuite/ruleengine/rules/set-neq-to-eq.rule"
        , "testsuite/ruleengine/rules/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        , "testsuite/ruleengine/rules/bogus/set-subseteq-to-quantified-1000-Plus.rule"
        , "testsuite/ruleengine/rules/bogus/set-subseteq-to-quantified-1000-Times.rule"
        ]
      )

    , (   "setNeq-9"
      ,   "testsuite/ruleengine/specs/setNeq.essence"
      , [ "testsuite/ruleengine/specs/out/testSetNeq9-" ++ show i ++ ".essence"
        | i <- [ (1::Int) .. 36 ]
        ]
      , [ "testsuite/ruleengine/rules/set-neq-to-eq.rule"
        , "testsuite/ruleengine/rules/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        , "testsuite/ruleengine/rules/bogus/set-subseteq-to-quantified-1000-Plus.rule"
        , "testsuite/ruleengine/rules/bogus/set-subseteq-to-quantified-1000-Minus.rule"
        , "testsuite/ruleengine/rules/bogus/set-subseteq-to-quantified-1000-Times.rule"
        , "testsuite/ruleengine/rules/bogus/set-subseteq-to-quantified-1000-Div.rule"
        , "testsuite/ruleengine/rules/bogus/set-subseteq-to-quantified-1000-Mod.rule"
        ]
      )


    , (   "setMax-1"
      ,   "testsuite/ruleengine/specs/setMax.essence"
      , [ "testsuite/ruleengine/specs/out/testSetMax1.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-max.rule"
        ]
      )

    , (   "setMax-2"
      ,   "testsuite/ruleengine/specs/setMax.essence"
      , [ "testsuite/ruleengine/specs/out/testSetMax2.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-max.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        ]
      )


    , (   "setMin-1"
      ,   "testsuite/ruleengine/specs/setMin.essence"
      , [ "testsuite/ruleengine/specs/out/testSetMin1.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-min.rule"
        ]
      )

    , (   "setMin-2"
      ,   "testsuite/ruleengine/specs/setMin.essence"
      , [ "testsuite/ruleengine/specs/out/testSetMin2.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-min.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        ]
      )


    , (   "setMaxInObj"
      ,   "testsuite/ruleengine/specs/setMaxInObj.essence"
      , [ "testsuite/ruleengine/specs/out/testSetMaxInObj.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-max.rule"
        , "testsuite/ruleengine/rules/set-min.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        ]
      )


    , ( "setIntersect-1"
      , "testsuite/ruleengine/specs/setIntersect.essence"
      , [ "testsuite/ruleengine/specs/out/testSetIntersect1.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        , "testsuite/ruleengine/rules/set-intersect-quantifier.rule"
        ]
      )

    , ( "setUnion-1"
      , "testsuite/ruleengine/specs/setUnion.essence"
      , [ "testsuite/ruleengine/specs/out/testSetUnion1.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        , "testsuite/ruleengine/rules/set-intersect-quantifier.rule"
        , "testsuite/ruleengine/rules/set-union-forAll.rule"
        , "testsuite/ruleengine/rules/set-union-exists.rule"
        ]
      )

    , ( "setMinus-1"
      , "testsuite/ruleengine/specs/setMinus.essence"
      , [ "testsuite/ruleengine/specs/out/testSetMinus1.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/set-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        , "testsuite/ruleengine/rules/set-minus-quantifier.rule"
        ]
      )

    , ( "setCard-1"
      , "testsuite/ruleengine/specs/setCard.essence"
      , [ "testsuite/ruleengine/specs/out/testSetCard1.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-card.rule"
        , "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        ]
      )

    , ( "setToMSet-1"
      , "testsuite/ruleengine/specs/setToMSet.essence"
      , [ "testsuite/ruleengine/specs/out/testSetToMSet1.essence"
        ]
      , [ "testsuite/ruleengine/rules/set-toMSet-to-quantified.rule"
        ]
      )



    , ( "msetIn-0"
      , "testsuite/ruleengine/specs/msetIn.essence"
      , [ ]
      , [ "testsuite/ruleengine/rules/set-in-to-quantified.rule"
        ]
      )

    , ( "msetIn-1"
      , "testsuite/ruleengine/specs/msetIn.essence"
      , [ "testsuite/ruleengine/specs/out/testMSetIn1.essence"
        ]
      , [ "testsuite/ruleengine/rules/mset-in.rule"
        ]
      )

    , ( "msetEq-0"
      , "testsuite/ruleengine/specs/msetEq.essence"
      , [ ]
      , [ "testsuite/ruleengine/rules/set-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/set-supseteq-to-subseteq.rule"
        ]
      )

    , ( "msetEq-1"
      , "testsuite/ruleengine/specs/msetEq.essence"
      , [ "testsuite/ruleengine/specs/out/testMSetEq1.essence"
        ]
      , [ "testsuite/ruleengine/rules/mset-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/mset-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/mset-subseteq-to-quantified.rule"
        ]
      )

    , ( "msetCard-1"
      , "testsuite/ruleengine/specs/msetCard.essence"
      , [ "testsuite/ruleengine/specs/out/testMSetCard1.essence"
        ]
      , [ "testsuite/ruleengine/rules/mset-card.rule"
        , "testsuite/ruleengine/rules/mset-in.rule"
        ]
      )


    , ( "msetIntersect1-1"
      , "testsuite/ruleengine/specs/msetIntersect1.essence"
      , [ "testsuite/ruleengine/specs/out/testMSetIntersect1-1.essence"
        ]
      , [ "testsuite/ruleengine/rules/mset-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/mset-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/mset-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/mset-in.rule"
        , "testsuite/ruleengine/rules/mset-intersect-forAll.rule"
        , "testsuite/ruleengine/rules/mset-intersect-exists.rule"
        , "testsuite/ruleengine/rules/mset-intersect-sum.rule"
        ]
      )

    , ( "msetIntersect1-2"
      , "testsuite/ruleengine/specs/msetIntersect1.essence"
      , [ "testsuite/ruleengine/specs/out/testMSetIntersect1-2.essence"
        ]
      , [ "testsuite/ruleengine/rules/mset-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/mset-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/mset-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/mset-in.rule"
        , "testsuite/ruleengine/rules/mset-intersect-forAll.rule"
        , "testsuite/ruleengine/rules/mset-intersect-exists.rule"
        , "testsuite/ruleengine/rules/mset-intersect-sum.rule"
        , "testsuite/ruleengine/rules/mset-freq-to-sum.rule"
        ]
      )

    , ( "msetIntersect2-1"
      , "testsuite/ruleengine/specs/msetIntersect2.essence"
      , [ "testsuite/ruleengine/specs/out/testMSetIntersect2-1.essence"
        ]
      , [ "testsuite/ruleengine/rules/mset-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/mset-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/mset-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/mset-in.rule"
        , "testsuite/ruleengine/rules/mset-intersect-forAll.rule"
        , "testsuite/ruleengine/rules/mset-intersect-exists.rule"
        , "testsuite/ruleengine/rules/mset-intersect-sum.rule"
        , "testsuite/ruleengine/rules/mset-card.rule"
        ]
      )

    , ( "msetIntersect2-2"
      , "testsuite/ruleengine/specs/msetIntersect2.essence"
      , [ "testsuite/ruleengine/specs/out/testMSetIntersect2-2.essence"
        ]
      , [ "testsuite/ruleengine/rules/mset-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/mset-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/mset-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/mset-in.rule"
        , "testsuite/ruleengine/rules/mset-intersect-forAll.rule"
        , "testsuite/ruleengine/rules/mset-intersect-exists.rule"
        , "testsuite/ruleengine/rules/mset-intersect-sum.rule"
        , "testsuite/ruleengine/rules/mset-card.rule"
        , "testsuite/ruleengine/rules/mset-freq-to-sum.rule"
        ]
      )

    , ( "msetIntersect3-1"
      , "testsuite/ruleengine/specs/msetIntersect3.essence"
      , [ "testsuite/ruleengine/specs/out/testMSetIntersect3-1.essence"
        ]
      , [ "testsuite/ruleengine/rules/mset-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/mset-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/mset-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/mset-in.rule"
        , "testsuite/ruleengine/rules/mset-intersect-forAll.rule"
        , "testsuite/ruleengine/rules/mset-intersect-exists.rule"
        , "testsuite/ruleengine/rules/mset-intersect-sum.rule"
        ]
      )

    , ( "msetIntersect3-2"
      , "testsuite/ruleengine/specs/msetIntersect3.essence"
      , [ "testsuite/ruleengine/specs/out/testMSetIntersect3-2.essence"
        ]
      , [ "testsuite/ruleengine/rules/mset-eq-to-subsets.rule"
        , "testsuite/ruleengine/rules/mset-supseteq-to-subseteq.rule"
        , "testsuite/ruleengine/rules/mset-subseteq-to-quantified.rule"
        , "testsuite/ruleengine/rules/mset-in.rule"
        , "testsuite/ruleengine/rules/mset-intersect-forAll.rule"
        , "testsuite/ruleengine/rules/mset-intersect-exists.rule"
        , "testsuite/ruleengine/rules/mset-intersect-sum.rule"
        , "testsuite/ruleengine/rules/mset-freq-to-sum.rule"
        ]
      )

    , ( "sets-chris1"
      , "testsuite/ruleengine/specs/sets-chris1.essence"
      , [ "testsuite/ruleengine/specs/sets-chris1.expected.essence" ]
      , allrules
      )

    , ( "sets-chris2"
      , "testsuite/ruleengine/specs/sets-chris2.essence"
      , [ "testsuite/ruleengine/specs/sets-chris2.expected.essence" ]
      , allrules
      )

    , ( "sets-chris3"
      , "testsuite/ruleengine/specs/sets-chris3.essence"
      , [ "testsuite/ruleengine/specs/sets-chris3.expected.essence" ]
      , allrules
      )

    , ( "sets-chris4"
      , "testsuite/ruleengine/specs/sets-chris4.essence"
      , [ "testsuite/ruleengine/specs/sets-chris4.expected.essence" ]
      , allrules
      )

    , ( "sets-chris5"
      , "testsuite/ruleengine/specs/sets-chris5.essence"
      , [ "testsuite/ruleengine/specs/sets-chris5.expected.essence" ]
      , allrules
      )

    , ( "sets-chris6"
      , "testsuite/ruleengine/specs/sets-chris6.essence"
      , [ "testsuite/ruleengine/specs/sets-chris6.expected.essence" ]
      , allrules
      )

    , ( "sets-chris7"
      , "testsuite/ruleengine/specs/sets-chris7.essence"
      , [ "testsuite/ruleengine/specs/sets-chris7.expected.essence" ]
      , allrules
      )

    , ( "sets-chris8"
      , "testsuite/ruleengine/specs/sets-chris8.essence"
      , [ "testsuite/ruleengine/specs/sets-chris8.expected.essence" ]
      , allrules
      )

    , ( "sets-chris9"
      , "testsuite/ruleengine/specs/sets-chris9.essence"
      , [ "testsuite/ruleengine/specs/sets-chris9.expected.essence" ]
      , allrules
      )

    , ( "sets-chris10"
      , "testsuite/ruleengine/specs/sets-chris10.essence"
      , [ "testsuite/ruleengine/specs/sets-chris10.expected.essence" ]
      , allrules
      )

    , ( "forAll-true"
      , "testsuite/ruleengine/specs/forAll-true.essence"
      , [ "testsuite/ruleengine/specs/forAll-true.expected.essence" ]
      , allrules
      )

    , ( "sets-ian1"
      , "testsuite/ruleengine/specs/sets-ian1.essence"
      , [ "testsuite/ruleengine/specs/sets-ian1.expected.essence" ]
      , allrules
      )

    , ( "sets-ian2"
      , "testsuite/ruleengine/specs/sets-ian2.essence"
      , [ "testsuite/ruleengine/specs/sets-ian2.expected.essence" ]
      , allrules
      )

    ]



allrules :: [FilePath]
allrules = map ("testsuite/ruleengine/rules/"++)
            [ "toInt-eq-0.rule"
            , "toInt-eq-1.rule"
            , "toInt-geq-1.rule"

            , "set-eq-to-subsets.rule"
            , "set-neq-to-eq.rule"

            , "set-in-to-quantified.rule"
            , "set-intersect-quantifier.rule"
            , "set-max.rule"
            , "set-min.rule"
            , "set-max-union.rule"
            , "set-min-union.rule"
            , "set-minus-quantifier.rule"
            , "set-subset-to-subsetEq.rule"
            , "set-subseteq-to-quantified.rule"
            , "set-supset-to-subset.rule"
            , "set-supseteq-to-subseteq.rule"
            , "set-toMSet-to-quantified.rule"
            , "set-union-exists.rule"
            , "set-union-forAll.rule"
            , "set-union-sum.rule"
            , "set-card.rule"

            , "mset-card.rule"
            , "mset-eq-to-subsets.rule"
            , "mset-freq-to-sum.rule"
            , "mset-in.rule"
            , "mset-intersect-exists.rule"
            , "mset-intersect-forAll.rule"
            , "mset-intersect-sum.rule"
            , "mset-subset-to-subsetEq.rule"
            , "mset-subseteq-to-quantified.rule"
            , "mset-supset-to-subset.rule"
            , "mset-supseteq-to-subseteq.rule"
            , "mset-toset-exists.rule"
            , "mset-toset-forAll.rule"
            , "mset-union-exists.rule"
            , "mset-union-forAll.rule"
            , "mset-union-sum.rule"

            ]
