{-# LANGUAGE OverloadedStrings #-}

module Conjure.Language.DomainSizeSpec ( spec ) where

-- conjure
import Language.E.Definition hiding ( Spec )
import Conjure.Language.DomainSize ( domainSizeConstant )

-- hspec
import Test.Hspec ( Spec, describe, it, shouldBe )


spec :: Spec
spec = describe "domain size calculation" $ do

    it "domain size of bool is 2" $
        domainSizeConstant DomainBool `shouldBe` Just 2

    it "domain size of int" $
        domainSizeConstant (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 100)]) `shouldBe` Just 100

    it "domain size of set of bool #1" $
        domainSizeConstant (DomainSet () SetAttrNone DomainBool) `shouldBe` Just 4

    it "domain size of set of bool #2" $
        let setOfSize n inner = DomainSet () (SetAttrSize n) inner
        in  domainSizeConstant (setOfSize (ConstantInt 2) DomainBool) `shouldBe` Just 1

