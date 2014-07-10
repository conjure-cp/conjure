{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.UpDownSpec ( spec ) where

-- conjure
import Language.E.Definition hiding ( Spec )
import Conjure.UpDown ( upDown, downDomain, downConstant, upConstant, UpDownError(..) )

-- mtl
import Control.Monad.Except ( runExceptT )

-- transformers
import Control.Monad.Trans.Class ( lift )

-- hspec
import Test.Hspec ( hspec, Spec, describe, it, shouldBe )

-- QuickCheck
import Test.QuickCheck ( property, (==>), NonNegative(..) )

import Data.Text ( Text )


-- spec :: Spec
-- spec = do
--
--     describe "enum" $ do
--
--         let enumValues = ["apple", "orange", "peach", "melon"]
--         let enumDomainDefn = DomainDefnEnum "fruits" enumValues
--         let enumDomain = DomainEnum enumDomainDefn []
--         let intDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)]
--
--         it "can convert enum domains into int domains" $ do
--             downDomain enumDomain `shouldBe` Right [intDomain]
--
--         it "can convert enum values into int values" $ do
--             let tDown n a b = downConstant enumDomain n a `shouldBe` b
--             tDown "x" (ConstantEnum enumDomainDefn "apple") (Right [("x", ConstantInt 1)])
--             tDown "x" (ConstantEnum enumDomainDefn "peach") (Right [("x", ConstantInt 3)])
--             tDown "x" (ConstantEnum enumDomainDefn "plum" ) (Left (ConstantDownError "[Conjure.UpDown.upDownEnum] This identifier isn't a member of the enum: plum"))
--
--         it "can reconstruct enum values from int values" $ do
--             let tUp n a b = upConstant enumDomain n a `shouldBe` b
--             tUp "x" [("x", ConstantInt 1)] (Right (ConstantEnum enumDomainDefn "apple"))
--             tUp "x" [("x", ConstantInt 3)] (Right (ConstantEnum enumDomainDefn "peach"))
--             tUp "x" [("x", ConstantInt 0)] (Left (ConstantUpError "[Conjure.UpDown.upDownEnum] Integer constant out of range for enum: 0"))
--
--         it "enum down&up with quickcheck" $ property $
--             let downAndUp c = do
--                     [(_,c')] <- downConstant enumDomain "x" c
--                     upConstant enumDomain "x" [("x", c')]
--             in  \ (NonNegative i) -> i < length enumValues ==>
--                     let e = ConstantEnum enumDomainDefn (enumValues !! i)
--                     in  downAndUp e `shouldBe` Right e
--
--     describe "Set Explicit" $ do
--
--         it "Set Explicit (downDomain)" $
--             let
--                 indexDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)]
--                 innerDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 9)]
--                 setDomain = DomainSet (Representation "Explicit") (SetAttrSize (ConstantInt 4)) innerDomain
--                 matrixDomain = DomainMatrix indexDomain innerDomain
--             in
--                 downDomain setDomain `shouldBe` Right [matrixDomain]
--
--         it "Set Explicit (downConstant)" $
--             let
--                 indexDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)]
--                 innerDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 9)]
--                 setDomain = DomainSet (Representation "Explicit") (SetAttrSize (ConstantInt 4)) innerDomain
--                 setConstant = ConstantSet [ConstantInt 1, ConstantInt 3, ConstantInt 5]
--                 matrixConstant = ConstantMatrix indexDomain [ConstantInt 1, ConstantInt 3, ConstantInt 5]
--             in
--                 downConstant setDomain "x" setConstant `shouldBe` Right [("x_Explicit", matrixConstant)]
--
--         it "Set Explicit (upConstant)" $
--             let
--                 indexDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)]
--                 innerDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 9)]
--                 setDomain = DomainSet (Representation "Explicit") (SetAttrSize (ConstantInt 4)) innerDomain
--                 setConstant = ConstantSet [ConstantInt 1, ConstantInt 3, ConstantInt 5]
--                 matrixConstant = ConstantMatrix indexDomain [ConstantInt 1, ConstantInt 3, ConstantInt 5]
--             in
--                 upConstant setDomain "x" [("x_Explicit", matrixConstant)] `shouldBe` Right setConstant
--
--         it "Set Explicit (downAndUp)" $
--             let
--                 innerDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 9)]
--                 setDomain = DomainSet (Representation "Explicit") (SetAttrSize (ConstantInt 4)) innerDomain
--                 setConstant = ConstantSet [ConstantInt 1, ConstantInt 3, ConstantInt 5]
--
--                 downAndUp c = do
--                     [(_, c')] <- downConstant setDomain "x" c
--                     upConstant setDomain "x" [("x_Explicit", c')]
--
--             in
--                 downAndUp setConstant `shouldBe` Right setConstant
--
--     describe "regression 1" $ do
--
--         it "regression 1 (downDomain)" $ do
--             downDomain
--                 (DomainSet
--                     (Representation "Explicit")
--                     (SetAttrSize (ConstantInt 1))
--                     (DomainTuple
--                         [ DomainBool
--                         , DomainInt [RangeBounded (ConstantInt 95) (ConstantInt 171)]
--                         , DomainInt [RangeBounded (ConstantInt 33) (ConstantInt 85)]])
--                 )
--                 `shouldBe` Right [ DomainMatrix (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 1)])
--                                     (DomainTuple
--                                         [ DomainBool
--                                         , DomainInt [RangeBounded (ConstantInt 95) (ConstantInt 171)]
--                                         , DomainInt [RangeBounded (ConstantInt 33) (ConstantInt 85)]])
--                                  ]
--
--         it "regression 1 (downConstant)" $ do
--             downConstant
--                 (DomainSet
--                     (Representation "Explicit")
--                     (SetAttrSize (ConstantInt 1))
--                     (DomainTuple
--                         [ DomainBool
--                         , DomainInt [RangeBounded (ConstantInt 95) (ConstantInt 171)]
--                         , DomainInt [RangeBounded (ConstantInt 33) (ConstantInt 85)]])
--                 )
--                 "x"
--                 (ConstantSet [ConstantTuple [ConstantBool False,ConstantInt 118,ConstantInt 79]])
--                 `shouldBe` Right [ ( "x_Explicit"
--                                    , ConstantMatrix (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 1)])
--                                         [ConstantTuple [ConstantBool False,ConstantInt 118,ConstantInt 79]]
--                                    )
--                                  ]
--
--         -- it "regression 1 (upConstant)" $ do
--         --     upConstant
--         --         (DomainSet
--         --             (Representation "Explicit")
--         --             (SetAttrSize (ConstantInt 1))
--         --             (DomainTuple
--         --                 [ DomainBool
--         --                 , DomainInt [RangeBounded (ConstantInt 95) (ConstantInt 171)]
--         --                 , DomainInt [RangeBounded (ConstantInt 33) (ConstantInt 85)]])
--         --         )
--         --         "x"
--         --         [ ( "x_Explicit"
--         --           , ConstantMatrix (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 1)])
--         --                 [ConstantTuple [ConstantBool False,ConstantInt 118,ConstantInt 79]]
--         --           )
--         --         ]
--         --         `shouldBe` Right (ConstantSet [ConstantTuple [ConstantBool False,ConstantInt 118,ConstantInt 79]])
--
--     describe "matrix of bool" $ do
--
--         let domainMatrixOfBool =
--                 DomainMatrix
--                     (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)])
--                     DomainBool
--
--         let constantMatrixOfBool =
--                 ConstantMatrix
--                     (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)])
--                     [ConstantBool False, ConstantBool False, ConstantBool True, ConstantBool True]
--
--         it "downDomain" $ do
--             downDomain
--                 domainMatrixOfBool
--                 `shouldBe` Right [domainMatrixOfBool]
--
--         it "downConstant" $ do
--             downConstant
--                 domainMatrixOfBool
--                 "x"
--                 constantMatrixOfBool
--                 `shouldBe` Right [("x", constantMatrixOfBool)]
--
--         it "upConstant" $ do
--             upConstant
--                 domainMatrixOfBool
--                 "x"
--                 [("x", constantMatrixOfBool)]
--                 `shouldBe` Right constantMatrixOfBool
--
--     describe "matrix of Set Explicit" $ do
--
--         let domainMatrixOfSet =
--                 DomainMatrix
--                     (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 2)])
--                     (DomainSet
--                         (Representation "Explicit")
--                         (SetAttrSize (ConstantInt 3))
--                         (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 9)]))
--
--         let constantMatrixOfSet =
--                 ConstantMatrix
--                     (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 2)])
--                     [ ConstantSet [ConstantInt 1, ConstantInt 3, ConstantInt 4]
--                     , ConstantSet [ConstantInt 1, ConstantInt 3, ConstantInt 5]
--                     ]
--
--         it "downDomain" $ do
--             downDomain
--                 domainMatrixOfSet
--                 `shouldBe` Right [domainMatrixOfSet]
--
--         it "downConstant" $ do
--             downConstant
--                 domainMatrixOfSet
--                 "x"
--                 constantMatrixOfSet
--                 `shouldBe` Right [("x", constantMatrixOfSet)]
--
--         it "upConstant" $ do
--             upConstant
--                 domainMatrixOfSet
--                 "x"
--                 [("x_Explicit", constantMatrixOfSet)]
--                 `shouldBe` Right constantMatrixOfSet
--
--     describe "set of set of int -- Explicit [Explicit [∅]]" $ do
--
--         let domainSetOfSet =
--                 DomainSet
--                     (Representation "Explicit")
--                     (SetAttrSize (ConstantInt 2))
--                     (DomainSet
--                         (Representation "Explicit")
--                         (SetAttrSize (ConstantInt 1))
--                         DomainBool)
--
--         let domainMatrixOfSet =
--                 DomainMatrix
--                     (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 2)])
--                     (DomainSet
--                         (Representation "Explicit")
--                         (SetAttrSize (ConstantInt 1))
--                         DomainBool)
--
--         let constantSetOfSet =
--                 ConstantSet
--                     [ ConstantSet [ConstantBool False]
--                     , ConstantSet [ConstantBool True]
--                     ]
--
--         let constantMatrixOfSet =
--                 ConstantMatrix
--                     (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 2)])
--                     [ ConstantSet [ConstantBool False]
--                     , ConstantSet [ConstantBool True]
--                     ]
--
--         it "downDomain" $ do
--             downDomain
--                 domainSetOfSet
--                 `shouldBe` Right [domainMatrixOfSet]
--
--         it "downConstant" $ do
--             downConstant
--                 domainSetOfSet
--                 "x"
--                 constantSetOfSet
--                 `shouldBe` Right [("x",constantMatrixOfSet)]
--
--         it "upConstant" $ do
--             upConstant
--                 domainSetOfSet
--                 "x"
--                 [("x_Explicit_Explicit", constantMatrixOfSet)]
--                 `shouldBe` Right constantSetOfSet


spec :: Spec
spec = describe "↓domains, ↓constants, ↑constants" $
    let
        process
            :: ( String
               , Domain Representation Constant
               , [Domain Representation Constant]
               , Text, Constant
               , [(Text, Constant)]
               )
            -> Spec
        process (name, highDomain, lowDomain, highName, highConstant, lowConstant) = do
            res <- runExceptT $ do
                (downDomain, _, _, downConstant, upConstant) <- upDown highDomain

                lowDomain' <- downDomain
                lift $ it (name ++ " ↓domain") $ lowDomain' `shouldBe` lowDomain

                lowConstant' <- downConstant highName highConstant
                lift $ it (name ++ " ↓constant") $ lowConstant' `shouldBe` lowConstant

                highConstant' <- upConstant highName lowConstant
                lift $ it (name ++ " ↑constant") $ highConstant' `shouldBe` highConstant

            case res of
                Left e -> fail (show e)
                Right _ -> return ()


    in mapM_ process
        [ ( "int #1"
          , DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)]
          , DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)]
          , "x"
          , ConstantInt 2
          , [("x", ConstantInt 2)]
          )
        ]













