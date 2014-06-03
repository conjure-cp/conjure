{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.UpDownSpec where

-- conjure
import Language.E.Imports
import Language.E.Definition hiding ( Spec )
import Conjure.UpDown ( downDomain, downConstant, upConstant, UpDownError(..) )

-- hspec
import Test.Hspec ( Spec, describe, it, shouldBe )

-- QuickCheck
import Test.QuickCheck ( property, (==>), NonNegative(..) )


spec :: Spec
spec = describe "enum up-down" $ do

    let enumValues = ["apple", "orange", "peach", "melon"]
    let enumDomainDefn = DomainDefnEnum "fruits" enumValues
    let enumDomain = DomainEnum enumDomainDefn []
    let intDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)]

    it "can convert enum domains into int domains" $ do
        downDomain NoRepresentation enumDomain `shouldBe` Right [intDomain]

    it "can convert enum values into int values" $ do
        let tDown a b = downConstant NoRepresentation enumDomain a `shouldBe` b
        tDown (ConstantEnum enumDomainDefn "apple") (Right [ConstantInt 1])
        tDown (ConstantEnum enumDomainDefn "peach") (Right [ConstantInt 3])
        tDown (ConstantEnum enumDomainDefn "plum" ) (Left (ConstantDownError "[Conjure.UpDown.upDownEnum] This identifier isn't a member of the enum: plum"))

    it "can reconstruct enum values from int values" $ do
        let tUp a b = upConstant NoRepresentation enumDomain a `shouldBe` b
        tUp [ConstantInt 1] (Right (ConstantEnum enumDomainDefn "apple"))
        tUp [ConstantInt 3] (Right (ConstantEnum enumDomainDefn "peach"))
        tUp [ConstantInt 0] (Left (ConstantUpError "[Conjure.UpDown.upDownEnum] Integer constant out of range for enum: 0"))

    it "enum down&up with quickcheck" $ property $
        let downAndUp = downConstant NoRepresentation enumDomain >=>
                        upConstant   NoRepresentation enumDomain
        in  \ (NonNegative i) -> i < length enumValues ==>
                let e = ConstantEnum enumDomainDefn (enumValues !! i)
                in  downAndUp e `shouldBe` Right e

    it "Set Explicit downDomain" $
        let
            sizeAttr = DANameValue "size" (ConstantInt 4)
            indexDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)]
            innerDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 9)]
            setDomain = DomainSet (DomainAttributes [sizeAttr]) innerDomain
            matrixDomain = DomainMatrix indexDomain innerDomain
        in
            downDomain (Representation "Explicit") setDomain `shouldBe` Right [matrixDomain]

    it "Set Explicit downConstant" $
        let
            sizeAttr = DANameValue "size" (ConstantInt 4)
            indexDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)]
            innerDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 9)]
            setDomain = DomainSet (DomainAttributes [sizeAttr]) innerDomain
            setConstant = ConstantSet [ConstantInt 1, ConstantInt 3, ConstantInt 5]
            matrixConstant = ConstantMatrix indexDomain [ConstantInt 1, ConstantInt 3, ConstantInt 5]
        in
            downConstant (Representation "Explicit") setDomain setConstant `shouldBe` Right [matrixConstant]

    it "Set Explicit upConstant" $
        let
            sizeAttr = DANameValue "size" (ConstantInt 4)
            indexDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)]
            innerDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 9)]
            setDomain = DomainSet (DomainAttributes [sizeAttr]) innerDomain
            setConstant = ConstantSet [ConstantInt 1, ConstantInt 3, ConstantInt 5]
            matrixConstant = ConstantMatrix indexDomain [ConstantInt 1, ConstantInt 3, ConstantInt 5]
        in
            upConstant (Representation "Explicit") setDomain [matrixConstant] `shouldBe` Right setConstant

    it "Set Explicit downAndUp" $
        let
            sizeAttr = DANameValue "size" (ConstantInt 4)
            innerDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 9)]
            setDomain = DomainSet (DomainAttributes [sizeAttr]) innerDomain
            setConstant = ConstantSet [ConstantInt 1, ConstantInt 3, ConstantInt 5]

            downAndUp = downConstant (Representation "Explicit") setDomain >=>
                        upConstant   (Representation "Explicit") setDomain

        in
            downAndUp setConstant `shouldBe` Right setConstant

