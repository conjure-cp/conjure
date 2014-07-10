{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.RepresentationsSpec where

-- conjure
import Language.E.Imports
import Language.E.Definition hiding ( Spec )
import Conjure.Representations
    ( ReprActions(down1, down1_, up1)
    , down, down_, up
    , primitive, tuple )

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

    describe "matrix of bool" $ do

        it "down1" $ down1Test primitive
            ( "x"
            , DomainMatrix (intDomain 1 3) DomainBool
            , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True]
            )
            Nothing

        it "down" $ downTest primitive
            ( "x"
            , DomainMatrix (intDomain 1 3) DomainBool
            , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True]
            )
          [ ( "x"
            , DomainMatrix (intDomain 1 3) DomainBool
            , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True]
            ) ]

        it "up" $ upTest primitive
            ( "x", DomainMatrix (intDomain 1 3) DomainBool )
           [("x", ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True])]
            ("x", ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True])

    describe "matrix of int" $ do
        it "down1" $ down1Test primitive
            ( "x"
            , DomainMatrix (intDomain 1 3) (intDomain 1 5)
            , ConstantMatrix (intDomain 1 3) [ConstantInt 2, ConstantInt 3, ConstantInt 5]
            )
            Nothing

        it "down" $ downTest primitive
            ( "x"
            , DomainMatrix (intDomain 1 3) (intDomain 1 5)
            , ConstantMatrix (intDomain 1 3) [ConstantInt 2, ConstantInt 3, ConstantInt 5]
            )
          [ ( "x"
            , DomainMatrix (intDomain 1 3) (intDomain 1 5)
            , ConstantMatrix (intDomain 1 3) [ConstantInt 2, ConstantInt 3, ConstantInt 5]
            ) ]

        it "up" $ upTest primitive
            ( "x", DomainMatrix (intDomain 1 3) (intDomain 1 5) )
           [("x", ConstantMatrix (intDomain 1 3) [ConstantInt 2, ConstantInt 3, ConstantInt 5])]
            ("x", ConstantMatrix (intDomain 1 3) [ConstantInt 2, ConstantInt 3, ConstantInt 5])

    describe "matrix 2d of bool" $ do

        it "down1" $ down1Test primitive
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainMatrix (intDomain 1 2) DomainBool)
            , ConstantMatrix (intDomain 1 3)
                [ ConstantMatrix (intDomain 1 2) [ConstantBool False, ConstantBool True ]
                , ConstantMatrix (intDomain 1 2) [ConstantBool True , ConstantBool False]
                , ConstantMatrix (intDomain 1 2) [ConstantBool True , ConstantBool True ]
                ]
            )
            Nothing

        it "down" $ downTest primitive
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainMatrix (intDomain 1 2) DomainBool)
            , ConstantMatrix (intDomain 1 3)
                [ ConstantMatrix (intDomain 1 2) [ConstantBool False, ConstantBool True ]
                , ConstantMatrix (intDomain 1 2) [ConstantBool True , ConstantBool False]
                , ConstantMatrix (intDomain 1 2) [ConstantBool True , ConstantBool True ]
                ]
            )
          [ ( "x"
            , DomainMatrix (intDomain 1 3) (DomainMatrix (intDomain 1 2) DomainBool)
            , ConstantMatrix (intDomain 1 3)
                [ ConstantMatrix (intDomain 1 2) [ConstantBool False, ConstantBool True ]
                , ConstantMatrix (intDomain 1 2) [ConstantBool True , ConstantBool False]
                , ConstantMatrix (intDomain 1 2) [ConstantBool True , ConstantBool True ]
                ]
            ) ]

        it "up" $ upTest primitive
            ( "x", DomainMatrix (intDomain 1 3) (DomainMatrix (intDomain 1 2) DomainBool) )
          [ ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantMatrix (intDomain 1 2) [ConstantBool False, ConstantBool True ]
                    , ConstantMatrix (intDomain 1 2) [ConstantBool True , ConstantBool False]
                    , ConstantMatrix (intDomain 1 2) [ConstantBool True , ConstantBool True ]
                    ]
            ) ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantMatrix (intDomain 1 2) [ConstantBool False, ConstantBool True ]
                    , ConstantMatrix (intDomain 1 2) [ConstantBool True , ConstantBool False]
                    , ConstantMatrix (intDomain 1 2) [ConstantBool True , ConstantBool True ]
                    ]
            )

    describe "matrix 2d of int" $ do

        it "down1" $ down1Test primitive
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainMatrix (intDomain 1 2) (intDomain 0 9))
            , ConstantMatrix (intDomain 1 3)
                [ ConstantMatrix (intDomain 1 2) [ConstantInt 3, ConstantInt 7]
                , ConstantMatrix (intDomain 1 2) [ConstantInt 2, ConstantInt 8]
                , ConstantMatrix (intDomain 1 2) [ConstantInt 0, ConstantInt 1]
                ]
            )
            Nothing

        it "down" $ downTest primitive
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainMatrix (intDomain 1 2) (intDomain 0 9))
            , ConstantMatrix (intDomain 1 3)
                [ ConstantMatrix (intDomain 1 2) [ConstantInt 3, ConstantInt 7]
                , ConstantMatrix (intDomain 1 2) [ConstantInt 2, ConstantInt 8]
                , ConstantMatrix (intDomain 1 2) [ConstantInt 0, ConstantInt 1]
                ]
            )
          [ ( "x"
            , DomainMatrix (intDomain 1 3) (DomainMatrix (intDomain 1 2) (intDomain 0 9))
            , ConstantMatrix (intDomain 1 3)
                [ ConstantMatrix (intDomain 1 2) [ConstantInt 3, ConstantInt 7]
                , ConstantMatrix (intDomain 1 2) [ConstantInt 2, ConstantInt 8]
                , ConstantMatrix (intDomain 1 2) [ConstantInt 0, ConstantInt 1]
                ]
            ) ]

        it "up" $ upTest primitive
            ( "x", DomainMatrix (intDomain 1 3) (DomainMatrix (intDomain 1 2) (intDomain 0 9)) )
          [ ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantMatrix (intDomain 1 2) [ConstantInt 3, ConstantInt 7]
                    , ConstantMatrix (intDomain 1 2) [ConstantInt 2, ConstantInt 8]
                    , ConstantMatrix (intDomain 1 2) [ConstantInt 0, ConstantInt 1]
                    ]
            ) ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantMatrix (intDomain 1 2) [ConstantInt 3, ConstantInt 7]
                    , ConstantMatrix (intDomain 1 2) [ConstantInt 2, ConstantInt 8]
                    , ConstantMatrix (intDomain 1 2) [ConstantInt 0, ConstantInt 1]
                    ]
            )

    describe "(bool, int)" $ do

        it "down1" $ down1Test tuple
            ( "x"
            , DomainTuple [DomainBool, intDomain 1 3]
            , ConstantTuple [ConstantBool False, ConstantInt 2]
            )
            (Just [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", intDomain 1 3, ConstantInt 2      )
                  ])

        it "down" $ downTest tuple
            ( "x"
            , DomainTuple [DomainBool, intDomain 1 3]
            , ConstantTuple [ConstantBool False, ConstantInt 2]
            )
            [ ( "x_1", DomainBool   , ConstantBool False )
            , ( "x_2", intDomain 1 3, ConstantInt 2      )
            ]

        it "up" $ upTest tuple
            ( "x", DomainTuple [DomainBool, intDomain 1 3] )
            [ ( "x_1", ConstantBool False )
            , ( "x_2", ConstantInt 2      )
            ]
            ( "x", ConstantTuple [ConstantBool False, ConstantInt 2] )



down1Test
    :: ReprActions (Either Doc)
    -> (Text, Domain Representation Constant, Constant)
    -> Maybe [(Text, Domain Representation Constant, Constant)]
    -> Expectation
down1Test repr high low' =
    case down1_ repr high of
        Left err -> expectationFailure (show err)
        Right low -> low `shouldBe` low'

downTest
    :: ReprActions (Either Doc)
    -> (Text, Domain Representation Constant, Constant)
    -> [(Text, Domain Representation Constant, Constant)]
    -> Expectation
downTest repr high lows' =
    case down_ repr high of
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


intDomain :: Int -> Int -> Domain r Constant
intDomain lb ub = DomainInt [RangeBounded (ConstantInt lb) (ConstantInt ub)]

