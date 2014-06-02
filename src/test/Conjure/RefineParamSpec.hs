{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.RefineParamSpec where

-- conjure
import Conjure.Language.Definition hiding ( Spec )
import Conjure.Language.Arbitrary ( AnyDomainTuple(..), AnyConstantTuple(..) )
import Conjure.RefineParam

-- base
import Control.Monad ( forM_ )

-- containers
import Data.Tree ( Tree(..) )

-- hspec
import Test.Hspec ( Spec, describe, it, shouldBe )

-- QuickCheck
import Test.QuickCheck ( property )


spec :: Spec
spec = describe "int parameter" $ do

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
                    (Node NoRepresentation [])
                    ("x", intDomain, intValue) `shouldBe`
                    Right [ ("x", intDomain, intValue) ]

    it "int parameters stay as is (quickcheck)" $ property $ \ ranges int ->
        let
            domain = DomainInt ranges
            constant = ConstantInt int
        in
            refineSingleParam (Node NoRepresentation [])
                ("x", domain, constant) `shouldBe`
                Right [ ("x", domain, constant) ]

    it "tuples of 2 ints" $ property $ \ ranges1 int1 ranges2 int2 ->
        let
            domain = DomainTuple [DomainInt ranges1, DomainInt ranges2]
            constant = ConstantTuple [ConstantInt int1, ConstantInt int2]
        in
            refineSingleParam (Node NoRepresentation [])
                ("x", domain, constant) `shouldBe`
                Right [ ("x", domain, constant) ]

    it "tuples of 3 ints" $ property $ \ ranges1 int1 ranges2 int2 ranges3 int3 ->
        let
            domain = DomainTuple [DomainInt ranges1, DomainInt ranges2, DomainInt ranges3]
            constant = ConstantTuple [ConstantInt int1, ConstantInt int2, ConstantInt int3]
        in
            refineSingleParam (Node NoRepresentation [])
                ("x", domain, constant) `shouldBe`
                Right [ ("x", domain, constant) ]

    it "arbitrary tuples" $ property $ \ (AnyDomainTuple domain) (AnyConstantTuple constant) ->
        refineSingleParam (Node NoRepresentation [])
            ("x", domain, constant) `shouldBe`
            Right [ ("x", domain, constant) ]



