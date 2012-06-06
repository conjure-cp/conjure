{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Testing.RuleEngine where

import Language.Core
import Language.Core.Middleware
import qualified Language.Core.Middleware.ReadFile            as ReadFile            ( worker )
import qualified Language.Core.Middleware.ParseSpec           as ParseSpec           ( worker )
import qualified Language.Core.Middleware.AtMostOneSuchThat   as AtMostOneSuchThat   ( worker )
-- import qualified Language.Core.Middleware.PrintSpec           as PrintSpec           ( worker )
import qualified Language.Core.Middleware.ApplyTransformation as ApplyTransformation ( worker )
import qualified Language.Core.Middleware.ParseRuleRefn       as ParseRuleRefn       ( worker )
import qualified Language.Core.Middleware.RuleRefnToFunction  as RuleRefnToFunction  ( worker )

import Prelude hiding ( mapM )
import Data.Traversable ( mapM )

import qualified Text.PrettyPrint as Pr

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit


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


loadAndApply :: FilePath -> [FilePath] -> IO [Language.Core.Spec]
loadAndApply b as = do

    inputRules <- forM as $ \ a ->
                    runMiddlewareIO a
                    $  ReadFile.worker
                    ~> ParseRuleRefn.worker
    let (mRefnFunc,_,logs1) = runMiddleware inputRules $ RuleRefnToFunction.worker

    mapM_ print $ prettyLog logs1

    refnFunc <- case mRefnFunc of
                    Left  e -> error $ show $ nestedToDoc e
                    Right x -> return x

    inputSpec <- runMiddlewareIO b
                        $  ReadFile.worker
                        ~> ParseSpec.worker

    let (mspecs,_,logs2) = runMiddleware inputSpec
                            $  ApplyTransformation.worker refnFunc
                            ~> mapM (mapM AtMostOneSuchThat.worker)

    mapM_ print $ prettyLog logs2

    specs' <- case mspecs of
                Left  e         -> error $ show $ nestedToDoc e
                Right Nothing   -> return []
                Right (Just xs) -> return xs

    mapM_ (print . pretty) specs'
    print $ length specs'

    return specs'


buildTests :: [(String, FilePath, [FilePath], [FilePath])] -> Test.Hspec.Monadic.Spec
buildTests params = describe "rule engine" $ do
    forM_ params $ \ (name, input, outputs, rules) -> do
        it ("testing " ++ name) $ do
            inputRules <- forM rules $ \ a ->
                            runMiddlewareIO a
                                $  ReadFile.worker
                                ~> ParseRuleRefn.worker

            let (mRefnFunc,_,logs1) = runMiddleware inputRules $ RuleRefnToFunction.worker

            mapM_ print $ prettyLog logs1

            refnFunc <- case mRefnFunc of
                            Left  e -> error $ show $ nestedToDoc e
                            Right x -> return x

            inputSpec <- runMiddlewareIO input
                            $  ReadFile.worker
                            ~> ParseSpec.worker

            let (mspecs,_,logs2) = runMiddleware inputSpec
                                    $  ApplyTransformation.worker refnFunc
                                    ~> mapM (mapM AtMostOneSuchThat.worker)

            mapM_ print $ prettyLog logs2

            specs' <- case mspecs of
                        Left  e         -> do
                            assertFailure $ show $ nestedToDoc e
                            return $ error "never here"
                        Right Nothing   -> return []
                        Right (Just xs) -> return xs

            mapM_ (print . pretty) specs'
            print $ length specs'

            outputSpecs <- forM outputs $ \ a ->
                            runMiddlewareIO a
                                $  ReadFile.worker
                                ~> ParseSpec.worker
                                ~> AtMostOneSuchThat.worker

            if length specs' == length outputSpecs
                then return ()
                else assertFailure $ unlines [ "different number of outputs generated."
                                             , show (length specs', length outputSpecs)
                                             ]

            forM_ (zip3 [(1::Int) ..] specs' outputSpecs) $ \ (i,a,b) ->
                if a == b
                    then return ()
                    -- else assertFailure
                    --         $ show
                    --         $ vcat $ map pretty specs'
                    else assertFailure
                            $ Pr.renderStyle Pr.style { Pr.lineLength = 260 }
                            $ "specs not equal"
                                <+> Pr.parens (stringToDoc $ show i)
                                <++> vcat [ "generated" <++> pretty a
                                          , "expected " <++> pretty b
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
        , "testsuite/ruleengine/quantifier-intersect.rule"
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
        , "testsuite/ruleengine/quantifier-intersect.rule"
        , "testsuite/ruleengine/forAll-union.rule"
        , "testsuite/ruleengine/exists-union.rule"
        ]
      )

    ]
