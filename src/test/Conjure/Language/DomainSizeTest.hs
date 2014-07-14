{-# LANGUAGE OverloadedStrings #-}

module Conjure.Language.DomainSizeTest ( tests ) where

-- conjure
import Language.E.Definition hiding ( Spec )
import Conjure.Language.DomainSize ( domainSizeConstant )

-- tasty
import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC


tests :: TestTree
tests = testGroup "domain size"
    [ testCase "domain size of bool is 2" $
        domainSizeConstant DomainBool @?= Right 2
    , testCase "domain size of int" $
        domainSizeConstant (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 100)]) @?= Right 100
    , testCase "domain size of set of bool #1" $
        domainSizeConstant (DomainSet () SetAttrNone DomainBool) @?= Right 4
    , testCase "domain size of set of bool #2" $
        let setOfSize n inner = DomainSet () (SetAttrSize n) inner
        in  domainSizeConstant (setOfSize (ConstantInt 2) DomainBool) @?= Right 1
    ]

