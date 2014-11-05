module Conjure.Language.DomainSizeTest ( tests ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Constant
import Conjure.Language.Domain
import Conjure.Language.DomainSize ( domainSizeConstant )

-- tasty
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )


tests :: TestTree
tests = testGroup "domainSize"
    [ testCase "domain size of bool is 2" $
        domainSizeConstant DomainBool @?= Right 2
    , testCase "domain size of int(1..100)" $
        domainSizeConstant (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 100)]) @?= Right 100
    , testCase "domain size of int(1,...,100)" $
        domainSizeConstant (DomainInt (map (RangeSingle . ConstantInt) [1 .. 100])) @?= Right 100
    , testCase "domain size of int(13)" $
        domainSizeConstant (DomainInt [RangeSingle (ConstantInt 13)]) @?= Right 1
    , testCase "domain size of int(13,1..100)" $
        domainSizeConstant (DomainInt [ RangeSingle (ConstantInt 13)
                                      , RangeBounded (ConstantInt 1) (ConstantInt 100)
                                      ]) @?= Right 100
    , testCase "domain size of int(113,1..100)" $
        domainSizeConstant (DomainInt [ RangeSingle (ConstantInt 113)
                                      , RangeBounded (ConstantInt 1) (ConstantInt 100)
                                      ]) @?= Right 101
    , testCase "domain size of set of bool #1" $
        domainSizeConstant (DomainSet () (SetAttr SizeAttrNone) DomainBool) @?= Right 4
    , testCase "domain size of set of bool #2" $
        let setOfSize n = DomainSet () (SetAttr (SizeAttrSize n))
        in  domainSizeConstant (setOfSize (ConstantInt 2) DomainBool) @?= Right 1
    ]

