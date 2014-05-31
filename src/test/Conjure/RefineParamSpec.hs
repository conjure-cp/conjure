{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.RefineParamSpec where

-- conjure
import Language.E.Definition hiding ( Spec )
import Conjure.RefineParam

-- base
import Control.Monad ( forM_ )

-- containers
import Data.Tree ( Tree(..) )

-- hspec
import Test.Hspec ( Spec, describe, it, shouldBe )
import Test.Hspec.QuickCheck ( property )

-- QuickCheck
import Test.QuickCheck ( verbose )


spec :: Spec
spec = describe "int parameter" $ do

    it "int parameters stay as is" $ do
        let intRange = [1..9]
        let intDomain1 = DomainInt []
        let intDomain2 = DomainInt [ RangeBounded (C (ConstantInt (minimum intRange)))
                                                  (C (ConstantInt (maximum intRange))) ]
        let intDomain3 = DomainInt [ RangeSingle (C (ConstantInt i)) | i <- intRange ]
        let intDomains = [intDomain1, intDomain2, intDomain3]
        let intValues  = [ ConstantInt i | i <- intRange ]

        forM_ intDomains $ \ intDomain ->
            forM_ intValues $ \ intValue ->
                refineSingleParam
                    (Node NoRepresentation [])
                    ("x", intDomain, intValue) `shouldBe`
                    Right [ ("x", intDomain, intValue) ]

    it "int parameters stay as is (quickcheck)" $ property $ verbose $ \ ranges int ->
        let
            domain = DomainInt ranges
            constant = ConstantInt int
        in
            refineSingleParam (Node NoRepresentation [])
                ("x", domain, constant) `shouldBe`
                Right [ ("x", domain, constant) ]

