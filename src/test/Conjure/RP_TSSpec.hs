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

-- hspec
import Test.Hspec ( Spec, describe, it, shouldBe )

-- QuickCheck
import Test.QuickCheck ( property )


allTheWay
    :: Text
    -> Domain Representation Constant
    -> Constant
    -> Spec
allTheWay name domain constant =
    it (show $ sep [ "allTheWay"
                   , "        name           :" <+> pretty name
                   , "        domain         :" <+> pretty domain
                   , "        constant       :" <+> pretty constant
                   ]) $
        let
            result = do
                low <- refineSingleParam (name, domain, constant)
                translateSingleSolution name domain [ (n,c) | (n,_,c) <- low ]
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

    allTheWay "x"
        (DomainSet
            (Representation "Explicit")
            (SetAttrSize (ConstantInt 3))
            (DomainInt [RangeBounded (ConstantInt 3) (ConstantInt 7)]))
        (ConstantSet [ConstantInt 3, ConstantInt 5, ConstantInt 6])

    allTheWay "x"
        (DomainSet
            (Representation "Explicit")
            (SetAttrSize (ConstantInt 2))
            (DomainSet
                (Representation "Explicit")
                (SetAttrSize (ConstantInt 1))
                DomainBool))
        (ConstantSet
            [ ConstantSet [ConstantBool False]
            , ConstantSet [ConstantBool True]
            ])

    allTheWay "x"
        (DomainMatrix
            (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 0)])
            DomainBool)
        (ConstantMatrix
            (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 0)])
            [])

    allTheWay "x"
        (DomainMatrix
            (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)])
            DomainBool)
        (ConstantMatrix
            (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)])
            [ ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False ])

    it "allTheWay (QuickCheck)" $ property $ \ (Name name) (AnyDomainAndConstant domain constant) ->
        let
            result = do
                low <- refineSingleParam (name, domain, constant)
                translateSingleSolution name domain [ (n,c) | (n,_,c) <- low ]
        in
            result `shouldBe` Right (name, constant)

