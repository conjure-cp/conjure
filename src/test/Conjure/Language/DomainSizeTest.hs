module Conjure.Language.DomainSizeTest ( tests ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Constant
import Conjure.Language.Type
import Conjure.Language.Domain
import Conjure.Language.DomainSizeOf ( domainSizeOf )

-- tasty
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )


domainSizeConstant :: (MonadFailDoc m ) => Domain () Constant -> m Integer
domainSizeConstant = domainSizeOf

tests :: TestTree
tests = testGroup "domainSize"
    [ testCase "domain size of bool is 2" $
        domainSizeConstant DomainBool @?= Right 2
    , testCase "domain size of int(1..100)" $
        domainSizeConstant (DomainInt TagInt [RangeBounded ((ConstantInt TagInt) 1) ((ConstantInt TagInt) 100)]) @?= Right 100
    , testCase "domain size of int(1,...,100)" $
        domainSizeConstant (DomainInt TagInt (map (RangeSingle . (ConstantInt TagInt)) [1 .. 100])) @?= Right 100
    , testCase "domain size of int(13)" $
        domainSizeConstant (DomainInt TagInt [RangeSingle ((ConstantInt TagInt) 13)]) @?= Right 1
    , testCase "domain size of int(13,1..100)" $
        domainSizeConstant (DomainInt TagInt [ RangeSingle ((ConstantInt TagInt) 13)
                                      , RangeBounded ((ConstantInt TagInt) 1) ((ConstantInt TagInt) 100)
                                      ]) @?= Right 100
    , testCase "domain size of int(113,1..100)" $
        domainSizeConstant (DomainInt TagInt [ RangeSingle ((ConstantInt TagInt) 113)
                                      , RangeBounded ((ConstantInt TagInt) 1) ((ConstantInt TagInt) 100)
                                      ]) @?= Right 101
    , testCase "domain size of set of bool #1" $
        domainSizeConstant (DomainSet () (SetAttr SizeAttr_None) DomainBool) @?= Right 4
    , testCase "domain size of set of bool #2" $
        let setOfSize n = DomainSet () (SetAttr (SizeAttr_Size n))
        in  domainSizeConstant (setOfSize ((ConstantInt TagInt) 2) DomainBool) @?= Right 1
    ]

