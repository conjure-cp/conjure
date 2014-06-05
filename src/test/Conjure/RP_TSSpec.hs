{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}

module Conjure.RP_TSSpec ( spec ) where

-- conjure
import Conjure.Language.Definition hiding ( Spec )
import Conjure.Language.Arbitrary ( AnyDomainAndConstant(..) )
import Conjure.RefineParam ( refineSingleParam )
import Conjure.TranslateSolution ( translateSingleSolution )
import Language.E.Imports
import Language.E.Pretty

-- containers
import Data.Tree ( Tree(..) )

-- hspec
import Test.Hspec ( Spec, describe, it, shouldBe )

-- QuickCheck
import Test.QuickCheck ( property )


allTheWay
    :: Text
    -> Domain Constant
    -> Constant
    -> Tree Representation
    -> Spec
allTheWay name domain constant representation =
    it (show $ sep [ "allTheWay"
                   , "        name           :" <+> pretty name
                   , "        domain         :" <+> pretty domain
                   , "        constant       :" <+> pretty constant
                   , "        representation :" <+> pretty representation
                   ]) $
        let
            result = do
                low <- refineSingleParam representation (name, domain, constant)
                translateSingleSolution name domain representation [ (n,c) | (n,_,c) <- low ]
        in
            result `shouldBe` Right (name, constant)


spec :: Spec
spec = describe "RefineParam & TranslateSolution" $ do

    let names = ["x", "x_y", "x_y_z"]

    forM_ names $ \ name ->
        allTheWay
            name
            (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 9)])
            (ConstantInt 3)
            (Node NoRepresentation [])

    forM_ names $ \ name ->
        allTheWay
            name
            (DomainTuple
                [ DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 9)]
                , DomainBool
                , DomainInt [RangeSingle (ConstantInt i) | i <- [1..9], odd i]
                ])
            (ConstantTuple
                [ ConstantInt 3
                , ConstantBool False
                , ConstantInt 7
                ])
            (Node NoRepresentation
                [ Node NoRepresentation []
                , Node NoRepresentation []
                , Node NoRepresentation []
                ])

    allTheWay "x"
        (DomainSet
            (DomainAttributes [DANameValue "size" (ConstantInt 3)])
            (DomainInt [RangeBounded (ConstantInt 3) (ConstantInt 7)]))
        (ConstantSet [ConstantInt 3, ConstantInt 5, ConstantInt 6])
        (Node (Representation "Explicit")
            [ Node NoRepresentation [] ])

    allTheWay "x"
        (DomainSet
            (DomainAttributes [DANameValue "size" (ConstantInt 2)])
            (DomainSet
                (DomainAttributes [DANameValue "size" (ConstantInt 1)])
                DomainBool))
        (ConstantSet
            [ ConstantSet [ConstantBool False]
            , ConstantSet [ConstantBool True]
            ])
        (Node (Representation "Explicit")
            [ Node (Representation "Explicit") [
                Node NoRepresentation [] ] ])

    allTheWay "x"
        (DomainMatrix
            (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)])
            DomainBool)
        (ConstantMatrix
            (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)])
            [ ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False ])
        (Node NoRepresentation
            [ Node NoRepresentation [] ])

    it "allTheWay (QuickCheck)" $ property $ \ (Name name) (AnyDomainAndConstant domain representation constant) ->
        let
            result = do
                low <- refineSingleParam representation (name, domain, constant)
                translateSingleSolution name domain representation [ (n,c) | (n,_,c) <- low ]
        in
            result `shouldBe` Right (name, constant)

