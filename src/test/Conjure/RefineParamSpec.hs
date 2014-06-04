{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}

module Conjure.RefineParamSpec ( spec ) where

-- conjure
import Conjure.Language.Definition hiding ( Spec )
import Conjure.RefineParam

-- base
import Control.Monad ( forM_ )
import Data.Monoid ( mappend )

-- containers
import Data.Tree ( Tree(..) )

-- hspec
import Test.Hspec ( Spec, describe, it, shouldBe )

-- QuickCheck
import Test.QuickCheck ( property )

-- text
import Data.Text ( pack )


spec :: Spec
spec = describe "refining parameters" $ do

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

    it "arbitrary tuples 0" $ do
        refineSingleParam (Node NoRepresentation [])
            ("x", DomainTuple [], ConstantTuple []) `shouldBe`
            Right []

    it "arbitrary tuples 1" $ do
        refineSingleParam (Node NoRepresentation [])
            ("x", DomainTuple [DomainBool], ConstantTuple [ConstantBool False]) `shouldBe`
            Right [ ("x_1", DomainBool, ConstantBool False) ]

    it "arbitrary tuples 2" $ do
        refineSingleParam (Node NoRepresentation [])
            ("x", DomainTuple [DomainBool,DomainBool], ConstantTuple [ConstantBool False,ConstantBool True]) `shouldBe`
            Right [ ("x_1", DomainBool, ConstantBool False)
                  , ("x_2", DomainBool, ConstantBool True)
                  ]

    it "arbitrary tuples (quickcheck)" $ property $ \ p -> do
        -- p is a list of pairs here. first component is the domain, second component is the constant.
        let (domains, constants) = unzip p        
        refineSingleParam (Node NoRepresentation [])
            ("x", DomainTuple domains, ConstantTuple constants) `shouldBe`
            Right [ ("x_" `mappend` pack (show i), d, c)
                  | i <- [1::Int ..]
                  | d <- domains
                  | c <- constants 
                  ]

    it "Set Explicit" $
        let
            sizeAttr = DANameValue "size" (ConstantInt 4)
            indexDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)]
            innerDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 9)]
            setDomain = DomainSet (DomainAttributes [sizeAttr]) innerDomain
            setConstant = ConstantSet [ConstantInt 1, ConstantInt 3, ConstantInt 5]
            matrixDomain = DomainMatrix indexDomain innerDomain
            matrixConstant = ConstantMatrix indexDomain [ConstantInt 1, ConstantInt 3, ConstantInt 5]
        in
            refineSingleParam (Node (Representation "Explicit") [])
                ("x", setDomain, setConstant) `shouldBe`
                Right [ ("x_Explicit", matrixDomain, matrixConstant) ]

    it "Set Explicit (nested)" $
        let
            sizeAttr1 = DANameValue "size" (ConstantInt 4)
            sizeAttr2 = DANameValue "size" (ConstantInt 3)
            indexDomain1 = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)]
            indexDomain2 = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 3)]
            innerDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 9)]
            setDomain = DomainSet (DomainAttributes [sizeAttr1])
                            (DomainSet (DomainAttributes [sizeAttr2]) innerDomain)
            setConstant = ConstantSet
                [ ConstantSet [ConstantInt 1, ConstantInt 3, ConstantInt 5]
                , ConstantSet [ConstantInt 1, ConstantInt 3, ConstantInt 6]
                , ConstantSet [ConstantInt 1, ConstantInt 4, ConstantInt 5]
                , ConstantSet [ConstantInt 1, ConstantInt 4, ConstantInt 6]
                ]
            matrixDomain = DomainMatrix indexDomain1 (DomainMatrix indexDomain2 innerDomain)
            matrixConstant = ConstantMatrix indexDomain1
                [ ConstantMatrix indexDomain2 [ConstantInt 1, ConstantInt 3, ConstantInt 5]
                , ConstantMatrix indexDomain2 [ConstantInt 1, ConstantInt 3, ConstantInt 6]
                , ConstantMatrix indexDomain2 [ConstantInt 1, ConstantInt 4, ConstantInt 5]
                , ConstantMatrix indexDomain2 [ConstantInt 1, ConstantInt 4, ConstantInt 6]
                ]
        in
            refineSingleParam (Node (Representation "Explicit") [Node (Representation "Explicit") []])
                ("x", setDomain, setConstant) `shouldBe`
                Right [ ("x_Explicit_Explicit", matrixDomain, matrixConstant) ]

