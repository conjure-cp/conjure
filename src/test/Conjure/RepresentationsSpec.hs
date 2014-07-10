{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.RepresentationsSpec where

-- conjure
import Language.E.Imports
import Language.E.Definition hiding ( Spec )
import Conjure.Representations ( ReprActions(down1, up), down, primitive )

-- hspec
import Test.Hspec ( Spec, describe, it, shouldBe, Expectation, expectationFailure )
-- import Test.Hspec ( hspec )

-- QuickCheck
-- import Test.QuickCheck ( property, (==>), NonNegative(..) )


spec :: Spec
spec = do

    describe "bool #1" $ do

        it "down1" $ down1Test primitive
            ("x", DomainBool, ConstantBool False)
            Nothing

        it "down" $ downTest primitive
            ("x", DomainBool, ConstantBool False)
           [("x", DomainBool, ConstantBool False)]

        it "up" $ upTest primitive
            ("x", DomainBool)
           [("x", ConstantBool False)]
            ("x", ConstantBool False)

    describe "bool #2" $ do

        it "down1" $ down1Test primitive
            ("x", DomainBool, ConstantBool True)
            Nothing

        it "down" $ downTest primitive
            ("x", DomainBool, ConstantBool True)
           [("x", DomainBool, ConstantBool True)]

        it "up" $ upTest primitive
            ("x", DomainBool)
           [("x", ConstantBool True)]
            ("x", ConstantBool True)

    describe "int #1" $ do

        it "down1" $ down1Test primitive
            ("x", DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)], ConstantInt 3)
            Nothing

        it "down" $ downTest primitive
            ("x", DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)], ConstantInt 3)
            [("x", DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)], ConstantInt 3)]

        it "up" $ upTest primitive
            ("x", DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)])
            [("x", ConstantInt 3)]
            ("x", ConstantInt 3)



down1Test
    :: ReprActions (Either Doc)
    -> (Text, Domain Representation Constant, Constant)
    -> Maybe [(Text, Domain Representation Constant, Constant)]
    -> Expectation
down1Test repr high low' =
    case down1 repr high of
        Left err -> expectationFailure (show err)
        Right low -> low `shouldBe` low'

downTest
    :: ReprActions (Either Doc)
    -> (Text, Domain Representation Constant, Constant)
    -> [(Text, Domain Representation Constant, Constant)]
    -> Expectation
downTest repr high lows' =
    case down repr high of
        Left err -> expectationFailure (show err)
        Right lows -> lows `shouldBe` lows'

upTest
    :: ReprActions (Either Doc)
    -> (Text, Domain Representation Constant)
    -> [(Text, Constant)]
    -> (Text, Constant)
    -> Expectation
upTest repr info lows high' =
    case up repr info lows of
        Left err -> expectationFailure (show err)
        Right high -> high `shouldBe` high'


