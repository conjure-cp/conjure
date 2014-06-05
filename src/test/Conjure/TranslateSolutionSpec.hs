{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}

module Conjure.TranslateSolutionSpec ( spec ) where

-- conjure
import Conjure.Language.Definition hiding ( Spec )
import Conjure.TranslateSolution

-- base
-- import Control.Monad ( forM_ )
import Data.Monoid ( mappend )

-- containers
import Data.Tree ( Tree(..) )

-- hspec
import Test.Hspec ( Spec, describe, it, shouldBe )

-- QuickCheck
-- import Test.QuickCheck ( property )

-- text
import Data.Text ( Text )


spec :: Spec
spec = describe "translating solutions" $ do

    it "int constants stay as is" $ do
        translateSingleSolution
            "x"
            (DomainInt [])
            (Node NoRepresentation []) 
            [("x", ConstantInt 1)] `shouldBe`
            Right ("x", ConstantInt 1)

    it "tuples" $ do
        translateSingleSolution
            "x"
            (DomainTuple [DomainInt [], DomainBool])
            (Node NoRepresentation [ Node NoRepresentation []
                                   , Node NoRepresentation []
                                   ])
            [ ("x_1", ConstantInt 1)
            , ("x_2", ConstantBool False)
            ] `shouldBe`
            Right ("x"
                  , ConstantTuple [ConstantInt 1, ConstantBool False]
                  )

    it "tuples nested 1 x"   $ nested1 "x"
    it "tuples nested 1 x_y" $ nested1 "x_y"
    it "tuples nested 2 x"   $ nested2 "x"
    it "tuples nested 2 x_y" $ nested2 "x_y"

    it "regression 1" $ do
        translateSingleSolution
            "x"
            (DomainSet (DomainAttributes [DANameValue (Name "size") (ConstantInt 1)])
                (DomainTuple
                    [ DomainBool
                    , DomainInt [RangeBounded (ConstantInt 95) (ConstantInt 171)]
                    , DomainInt [RangeBounded (ConstantInt 33) (ConstantInt 85)]
                    ]))
            (Node (Representation "Explicit")
                [ Node NoRepresentation
                    [ Node NoRepresentation []
                    , Node NoRepresentation []
                    , Node NoRepresentation []
                    ]])
            [ ( "x_Explicit_1"
              , ConstantMatrix (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 1)]) [ConstantBool False]
              )
            , ( "x_Explicit_2"
              , ConstantMatrix (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 1)]) [ConstantInt 118]
              )
            , ( "x_Explicit_3"
              , ConstantMatrix (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 1)]) [ConstantInt 79]
              )
            ] `shouldBe`
            Right ( "x"
                  , ConstantSet [ConstantTuple [ConstantBool False,ConstantInt 118,ConstantInt 79]]
                  )


nested1 :: Text -> IO ()
nested1 x =
    translateSingleSolution
        x
        (DomainTuple
            [ DomainInt []
            , DomainTuple
                [ DomainBool
                , DomainBool
                ]
            ])
        (Node NoRepresentation
            [ Node NoRepresentation []
            , Node NoRepresentation
                [ Node NoRepresentation []
                , Node NoRepresentation []
                ]
            ])
        [ (x `mappend` "_1"  , ConstantInt 1)
        , (x `mappend` "_2_1", ConstantBool False)
        , (x `mappend` "_2_2", ConstantBool True)
        ] `shouldBe`
        Right ( x
              , ConstantTuple
                  [ ConstantInt 1
                  , ConstantTuple
                      [ ConstantBool False
                      , ConstantBool True
                      ]
                  ]
              )

nested2 :: Text -> IO ()
nested2 x =
    translateSingleSolution
        x
        (DomainTuple
            [ DomainTuple
                [ DomainBool
                , DomainBool
                ]
            , DomainInt []
            ])
        (Node NoRepresentation
            [ Node NoRepresentation
                [ Node NoRepresentation []
                , Node NoRepresentation []
                ]
            , Node NoRepresentation []
            ])
        [ (x `mappend` "_1_1", ConstantBool False)
        , (x `mappend` "_1_2", ConstantBool True)
        , (x `mappend` "_2"  , ConstantInt 1)
        ] `shouldBe`
        Right ( x
              , ConstantTuple
                  [ ConstantTuple
                      [ ConstantBool False
                      , ConstantBool True
                      ]
                  , ConstantInt 1
                  ]
              )

