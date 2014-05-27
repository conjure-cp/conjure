{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.UpDownSpec where

-- conjure
import Language.E.Imports
import Language.E.Definition hiding ( Spec )
import Language.E.TH
import Conjure.UpDown

-- hspec
import Test.Hspec ( Spec, describe, it, shouldBe )
import Test.Hspec.QuickCheck ( property )

-- QuickCheck
import Test.QuickCheck ( NonNegative(..), verbose, (==>) )


spec :: Spec
spec = describe "enum up-down" $ do

    let enumValues = ["apple", "orange", "peach", "melon"]
    let enumDomainDefn = DomainDefnEnum "fruits" enumValues
    let enumDomain = DomainEnum enumDomainDefn []
    let intDomain = DomainInt [RangeBounded [eMake| 1 |] [eMake| 4 |]]

    it "can convert enum domains into int domains" $ do
        downDomain NoRepresentation enumDomain `shouldBe` Right intDomain

    it "can convert enum values into int values" $ do
        downValue NoRepresentation enumDomain [eMake| apple |] `shouldBe` Right [eMake| 1 |]
        downValue NoRepresentation enumDomain [eMake| peach |] `shouldBe` Right [eMake| 3 |]
        downValue NoRepresentation enumDomain [eMake| plum  |] `shouldBe` Left (ValueDownError "This identifier isn't a member of the enum: plum")

    it "can reconstruct enum values from int values" $ do
        upValue NoRepresentation enumDomain [eMake| 1 |] `shouldBe` Right [eMake| apple |]
        upValue NoRepresentation enumDomain [eMake| 3 |] `shouldBe` Right [eMake| peach |]
        upValue NoRepresentation enumDomain [eMake| 0 |] `shouldBe` Left (ValueUpError "Integer value out of range for enum: 0")

    it "enum down&up with quickcheck" $ property $ verbose $
        let downAndUp = downValue NoRepresentation enumDomain >=> upValue NoRepresentation enumDomain
        in  \ (NonNegative i) -> i < length enumValues ==>
                let e = [xMake| reference := [Prim (S (enumValues !! i))] |]
                in  downAndUp e == Right e

