{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}

module Conjure.RefineParamSpec ( spec ) where

-- conjure
import Conjure.Language.Definition hiding ( Spec )
import Conjure.RefineParam

-- base
import Control.Monad ( forM_ )

-- hspec
import Test.Hspec ( Spec, describe, it, shouldBe )

-- QuickCheck
import Test.QuickCheck ( property )


spec :: Spec
spec = describe "refining parameters" $ do

    it "int parameters stay as is" $ do
        let intRange = [1..9]
        let intDomain1 = DomainInt []
        let intDomain2 = DomainInt [ RangeBounded (ConstantInt (minimum intRange))
                                                  (ConstantInt (maximum intRange)) ]
        let intDomain3 = DomainInt [ RangeSingle (ConstantInt i) | i <- intRange ]
        let intDomains = [intDomain1, intDomain2, intDomain3]
        let intValues  = [ ConstantInt i | i <- intRange ]

        forM_ intDomains $ \ intDomain ->
            forM_ intValues $ \ intValue ->
                refineSingleParam
                    ("x", intDomain, intValue) `shouldBe`
                    Right [ ("x", intDomain, intValue) ]

    it "int parameters stay as is (quickcheck)" $ property $ \ ranges int ->
        let
            domain = DomainInt ranges
            constant = ConstantInt int
        in
            refineSingleParam
                ("x", domain, constant) `shouldBe`
                Right [ ("x", domain, constant) ]

    it "arbitrary tuples 0" $ do
        refineSingleParam
            ("x", DomainTuple [], ConstantTuple []) `shouldBe`
            Right []

    it "arbitrary tuples 1" $ do
        refineSingleParam
            ("x", DomainTuple [DomainBool], ConstantTuple [ConstantBool False]) `shouldBe`
            Right [ ("x_1", DomainBool, ConstantBool False) ]

    it "arbitrary tuples 2" $ do
        refineSingleParam
            ("x", DomainTuple [DomainBool,DomainBool], ConstantTuple [ConstantBool False,ConstantBool True]) `shouldBe`
            Right [ ("x_1", DomainBool, ConstantBool False)
                  , ("x_2", DomainBool, ConstantBool True)
                  ]

    it "Set Explicit" $
        let
            indexDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)]
            innerDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 9)]
            setDomain = DomainSet (Representation "Explicit") (SetAttrSize (ConstantInt 4)) innerDomain
            setConstant = ConstantSet [ConstantInt 1, ConstantInt 3, ConstantInt 5]
            matrixDomain = DomainMatrix indexDomain innerDomain
            matrixConstant = ConstantMatrix indexDomain [ConstantInt 1, ConstantInt 3, ConstantInt 5]
        in
            refineSingleParam
                ("x", setDomain, setConstant) `shouldBe`
                Right [ ("x_Explicit", matrixDomain, matrixConstant) ]

    it "Set Explicit (nested) #1" $
        let
            indexDomain1 = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)]
            indexDomain2 = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 3)]
            innerDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 9)]
            setDomain = DomainSet
                            (Representation "Explicit")
                            (SetAttrSize (ConstantInt 4))
                            (DomainSet
                                (Representation "Explicit")
                                (SetAttrSize (ConstantInt 3))
                                innerDomain)
            setConstant = ConstantSet
                [ ConstantSet [ConstantInt 1, ConstantInt 3, ConstantInt 5]
                , ConstantSet [ConstantInt 1, ConstantInt 3, ConstantInt 6]
                , ConstantSet [ConstantInt 1, ConstantInt 4, ConstantInt 5]
                , ConstantSet [ConstantInt 1, ConstantInt 4, ConstantInt 6]
                ]
            matrixDomain = DomainMatrix indexDomain1 (DomainMatrix indexDomain2 innerDomain)
            matrixConstant = ConstantMatrix indexDomain1
                [ ConstantMatrix indexDomain2 [ConstantInt 1, ConstantInt 3, ConstantInt 5]
                , ConstantMatrix indexDomain2 [ConstantInt 1, ConstantInt 3, ConstantInt 6]
                , ConstantMatrix indexDomain2 [ConstantInt 1, ConstantInt 4, ConstantInt 5]
                , ConstantMatrix indexDomain2 [ConstantInt 1, ConstantInt 4, ConstantInt 6]
                ]
        in
            refineSingleParam
                ("x", setDomain, setConstant) `shouldBe`
                Right [ ("x_Explicit_Explicit", matrixDomain, matrixConstant) ]

    it "Set Explicit (nested) #2" $
        let
            indexDomain1 = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 2)]
            indexDomain2 = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 1)]
            innerDomain = DomainBool
            setDomain = DomainSet
                            (Representation "Explicit")
                            (SetAttrSize (ConstantInt 2))
                            (DomainSet
                                (Representation "Explicit")
                                (SetAttrSize (ConstantInt 1))
                                innerDomain)
            setConstant = ConstantSet
                [ ConstantSet [ConstantBool False]
                , ConstantSet [ConstantBool True]
                ]
            matrixDomain = DomainMatrix indexDomain1 (DomainMatrix indexDomain2 innerDomain)
            matrixConstant = ConstantMatrix indexDomain1
                [ ConstantMatrix indexDomain2 [ConstantBool False]
                , ConstantMatrix indexDomain2 [ConstantBool True]
                ]
        in
            refineSingleParam
                ("x", setDomain, setConstant) `shouldBe`
                Right [ ("x_Explicit_Explicit", matrixDomain, matrixConstant) ]

    it "regression 1" $ do
        refineSingleParam
            ( "x", DomainSet
                        (Representation "Explicit")
                        (SetAttrSize (ConstantInt 1))
                        (DomainTuple
                            [ DomainBool
                            , DomainInt [RangeBounded (ConstantInt 95) (ConstantInt 171)]
                            , DomainInt [RangeBounded (ConstantInt 33) (ConstantInt 85)]])
                 , ConstantSet [ConstantTuple [ConstantBool False,ConstantInt 118,ConstantInt 79]]
            )
            `shouldBe` Right [ ( "x_Explicit_1"
                               , DomainMatrix (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 1)]) DomainBool
                               , ConstantMatrix (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 1)]) [ConstantBool False]
                               )
                             , ( "x_Explicit_2"
                               , DomainMatrix (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 1)]) (DomainInt [RangeBounded (ConstantInt 95) (ConstantInt 171)])
                               , ConstantMatrix (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 1)]) [ConstantInt 118]
                               )
                             , ( "x_Explicit_3"
                               , DomainMatrix (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 1)]) (DomainInt [RangeBounded (ConstantInt 33) (ConstantInt 85)])
                               , ConstantMatrix (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 1)]) [ConstantInt 79]
                               )
                             ]

