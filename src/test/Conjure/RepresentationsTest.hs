{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Conjure.RepresentationsTest ( tests ) where

-- conjure
import Language.E.Imports
import Language.E.Definition hiding ( Spec )
import Language.E.Pretty
import Conjure.Representations ( down_, up, down1_, up1, dispatch )

-- tasty
import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC


tests :: TestTree
tests = testGroup "representations"

    [ testGroup "bool #1"
        [ testCase "down1" $ down1Test
            ("x", DomainBool, ConstantBool False)
            Nothing
        , testCase "down" $ downTest
            ("x", DomainBool, ConstantBool False)
           [("x", DomainBool, ConstantBool False)]
        , testCase "up1" $ up1Test
            ("x", DomainBool)
           [("x", ConstantBool False)]
            ("x", ConstantBool False)
        , testCase "up" $ upTest
            ("x", DomainBool)
           [("x", ConstantBool False)]
            ("x", ConstantBool False)
        ]

    , testGroup "bool #2"
        [ testCase "down1" $ down1Test
            ("x", DomainBool, ConstantBool True)
            Nothing
        , testCase "down" $ downTest
            ("x", DomainBool, ConstantBool True)
           [("x", DomainBool, ConstantBool True)]
        , testCase "up1" $ up1Test
            ("x", DomainBool)
           [("x", ConstantBool True)]
            ("x", ConstantBool True)
        , testCase "up" $ upTest
            ("x", DomainBool)
           [("x", ConstantBool True)]
            ("x", ConstantBool True)
        ]

    , testGroup "int #1"
        [ testCase "down1" $ down1Test
            ("x", DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)], ConstantInt 3)
            Nothing
        , testCase "down" $ downTest
            ("x", DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)], ConstantInt 3)
           [("x", DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)], ConstantInt 3)]
        , testCase "up1" $ up1Test
            ("x", DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)])
           [("x", ConstantInt 3)]
            ("x", ConstantInt 3)
        , testCase "up" $ upTest
            ("x", DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)])
           [("x", ConstantInt 3)]
            ("x", ConstantInt 3)
        ]

    , testGroup "matrix of bool"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainMatrix (intDomain 1 3) DomainBool
            , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True]
            )
            Nothing
        , testCase "down" $ downTest
            ( "x"
            , DomainMatrix (intDomain 1 3) DomainBool
            , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True]
            )
          [ ( "x"
            , DomainMatrix (intDomain 1 3) DomainBool
            , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True]
            ) ]
        , testCase "up1" $ up1Test
            ( "x", DomainMatrix (intDomain 1 3) DomainBool )
           [("x", ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True])]
            ("x", ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True])
        , testCase "up" $ upTest
            ( "x", DomainMatrix (intDomain 1 3) DomainBool )
           [("x", ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True])]
            ("x", ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True])
        ]

    , testGroup "matrix of int"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainMatrix (intDomain 1 3) (intDomain 1 5)
            , ConstantMatrix (intDomain 1 3) [ConstantInt 2, ConstantInt 3, ConstantInt 5]
            )
            Nothing

        , testCase "down" $ downTest
            ( "x"
            , DomainMatrix (intDomain 1 3) (intDomain 1 5)
            , ConstantMatrix (intDomain 1 3) [ConstantInt 2, ConstantInt 3, ConstantInt 5]
            )
          [ ( "x"
            , DomainMatrix (intDomain 1 3) (intDomain 1 5)
            , ConstantMatrix (intDomain 1 3) [ConstantInt 2, ConstantInt 3, ConstantInt 5]
            ) ]

        , testCase "up1" $ up1Test
            ( "x", DomainMatrix (intDomain 1 3) (intDomain 1 5) )
           [("x", ConstantMatrix (intDomain 1 3) [ConstantInt 2, ConstantInt 3, ConstantInt 5])]
            ("x", ConstantMatrix (intDomain 1 3) [ConstantInt 2, ConstantInt 3, ConstantInt 5])

        , testCase "up" $ upTest
            ( "x", DomainMatrix (intDomain 1 3) (intDomain 1 5) )
           [("x", ConstantMatrix (intDomain 1 3) [ConstantInt 2, ConstantInt 3, ConstantInt 5])]
            ("x", ConstantMatrix (intDomain 1 3) [ConstantInt 2, ConstantInt 3, ConstantInt 5])
        ]

    , testGroup "matrix 2d of bool"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainMatrix (intDomain 1 2) DomainBool)
            , ConstantMatrix (intDomain 1 3)
                [ ConstantMatrix (intDomain 1 2) [ConstantBool False, ConstantBool True ]
                , ConstantMatrix (intDomain 1 2) [ConstantBool True , ConstantBool False]
                , ConstantMatrix (intDomain 1 2) [ConstantBool True , ConstantBool True ]
                ]
            )
            Nothing

        , testCase "down" $ downTest
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

        , testCase "up1" $ up1Test
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

        , testCase "up" $ upTest
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
        ]

    , testGroup "matrix 2d of int"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainMatrix (intDomain 1 2) (intDomain 0 9))
            , ConstantMatrix (intDomain 1 3)
                [ ConstantMatrix (intDomain 1 2) [ConstantInt 3, ConstantInt 7]
                , ConstantMatrix (intDomain 1 2) [ConstantInt 2, ConstantInt 8]
                , ConstantMatrix (intDomain 1 2) [ConstantInt 0, ConstantInt 1]
                ]
            )
            Nothing

        , testCase "down" $ downTest
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

        , testCase "up1" $ up1Test
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

        , testCase "up" $ upTest
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
        ]

    , testGroup "(bool, int)"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainTuple [DomainBool, intDomain 1 3]
            , ConstantTuple [ConstantBool False, ConstantInt 2]
            )
            (Just [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", intDomain 1 3, ConstantInt 2      )
                  ])

        , testCase "down" $ downTest
            ( "x"
            , DomainTuple [DomainBool, intDomain 1 3]
            , ConstantTuple [ConstantBool False, ConstantInt 2]
            )
            [ ( "x_1", DomainBool   , ConstantBool False )
            , ( "x_2", intDomain 1 3, ConstantInt 2      )
            ]

        , testCase "up1" $ up1Test
            ( "x", DomainTuple [DomainBool, intDomain 1 3] )
            [ ( "x_1", ConstantBool False )
            , ( "x_2", ConstantInt 2      )
            ]
            ( "x", ConstantTuple [ConstantBool False, ConstantInt 2] )

        , testCase "up" $ upTest
            ( "x", DomainTuple [DomainBool, intDomain 1 3] )
            [ ( "x_1", ConstantBool False )
            , ( "x_2", ConstantInt 2      )
            ]
            ( "x", ConstantTuple [ConstantBool False, ConstantInt 2] )
        ]

    , testGroup "(bool, int, bool)"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainTuple [DomainBool, intDomain 1 3, DomainBool]
            , ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True]
            )
            (Just [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", intDomain 1 3, ConstantInt 2      )
                  , ( "x_3", DomainBool   , ConstantBool True  )
                  ])

        , testCase "down" $ downTest
            ( "x"
            , DomainTuple [DomainBool, intDomain 1 3, DomainBool]
            , ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True]
            )
            [ ( "x_1", DomainBool   , ConstantBool False )
            , ( "x_2", intDomain 1 3, ConstantInt 2      )
            , ( "x_3", DomainBool   , ConstantBool True  )
            ]

        , testCase "up1" $ up1Test
            ( "x", DomainTuple [DomainBool, intDomain 1 3, DomainBool] )
            [ ( "x_1", ConstantBool False )
            , ( "x_2", ConstantInt 2      )
            , ( "x_3", ConstantBool True  )
            ]
            ( "x", ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True] )

        , testCase "up" $ upTest
            ( "x", DomainTuple [DomainBool, intDomain 1 3, DomainBool] )
            [ ( "x_1", ConstantBool False )
            , ( "x_2", ConstantInt 2      )
            , ( "x_3", ConstantBool True  )
            ]
            ( "x", ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True] )
        ]

    , testGroup "((bool, int), bool)"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool]
            , ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True]
            )
            (Just [ ( "x_1" , DomainTuple [DomainBool, intDomain 1 3], ConstantTuple [ConstantBool False, ConstantInt 2] )
                  , ( "x_2" , DomainBool, ConstantBool True )
                  ])
        , testCase "down" $ downTest
            ( "x"
            , DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool]
            , ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True]
            )
            [ ( "x_1_1", DomainBool   , ConstantBool False )
            , ( "x_1_2", intDomain 1 3, ConstantInt 2      )
            , ( "x_2"  , DomainBool   , ConstantBool True  )
            ]

        , testCase "up1" $ up1Test
            ( "x", DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool] )
            [ ( "x_1", ConstantTuple [ConstantBool False, ConstantInt 2] )
            , ( "x_2", ConstantBool True  )
            ]
            ( "x", ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True] )

        , testCase "up" $ upTest
            ( "x", DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool] )
            [ ( "x_1_1", ConstantBool False )
            , ( "x_1_2", ConstantInt 2      )
            , ( "x_2"  , ConstantBool True  )
            ]
            ( "x", ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True] )
        ]

    , testGroup "(bool, (int, bool))"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool]]
            , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantBool True]]
            )
            (Just [ ( "x_1" , DomainBool, ConstantBool False )
                  , ( "x_2" , DomainTuple [intDomain 1 3, DomainBool], ConstantTuple [ConstantInt 2, ConstantBool True] )
                  ])
        , testCase "down" $ downTest
            ( "x"
            , DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool]]
            , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantBool True]]
            )
            [ ( "x_1"  , DomainBool   , ConstantBool False )
            , ( "x_2_1", intDomain 1 3, ConstantInt 2      )
            , ( "x_2_2", DomainBool   , ConstantBool True  )
            ]

        , testCase "up1" $ up1Test
            ( "x", DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool]] )
            [ ( "x_1", ConstantBool False )
            , ( "x_2", ConstantTuple [ConstantInt 2, ConstantBool True] )
            ]
            ( "x", ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantBool True]] )

        , testCase "up" $ upTest
            ( "x", DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool]] )
            [ ( "x_1"  , ConstantBool False )
            , ( "x_2_1", ConstantInt 2      )
            , ( "x_2_2", ConstantBool True  )
            ]
            ( "x", ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantBool True]] )
        ]

    , testGroup "(bool, int, bool, int)"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainTuple [DomainBool, intDomain 1 3, DomainBool, intDomain 2 5]
            , ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True, ConstantInt 4]
            )
            (Just [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", intDomain 1 3, ConstantInt 2      )
                  , ( "x_3", DomainBool   , ConstantBool True  )
                  , ( "x_4", intDomain 2 5, ConstantInt 4      )
                  ])
        , testCase "down" $ downTest
            ( "x"
            , DomainTuple [DomainBool, intDomain 1 3, DomainBool, intDomain 2 5]
            , ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True, ConstantInt 4]
            )
            [ ( "x_1", DomainBool   , ConstantBool False )
            , ( "x_2", intDomain 1 3, ConstantInt 2      )
            , ( "x_3", DomainBool   , ConstantBool True  )
            , ( "x_4", intDomain 2 5, ConstantInt 4      )
            ]
        , testCase "up1" $ up1Test
            ( "x", DomainTuple [DomainBool, intDomain 1 3, DomainBool, intDomain 2 5] )
            [ ( "x_1", ConstantBool False )
            , ( "x_2", ConstantInt 2      )
            , ( "x_3", ConstantBool True  )
            , ( "x_4", ConstantInt 4      )
            ]
            ( "x", ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True, ConstantInt 4] )
        , testCase "up" $ upTest
            ( "x", DomainTuple [DomainBool, intDomain 1 3, DomainBool, intDomain 2 5] )
            [ ( "x_1", ConstantBool False )
            , ( "x_2", ConstantInt 2      )
            , ( "x_3", ConstantBool True  )
            , ( "x_4", ConstantInt 4      )
            ]
            ( "x", ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True, ConstantInt 4] )
        ]

    , testGroup "((bool, int), (bool, int))"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainTuple [DomainBool, intDomain 2 5]]
            , ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantTuple [ConstantBool True, ConstantInt 4]]
            )
            (Just [ ( "x_1", DomainTuple [DomainBool, intDomain 1 3], ConstantTuple [ConstantBool False, ConstantInt 2] )
                  , ( "x_2", DomainTuple [DomainBool, intDomain 2 5], ConstantTuple [ConstantBool True , ConstantInt 4] )
                  ])
        , testCase "down" $ downTest
            ( "x"
            , DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainTuple [DomainBool, intDomain 2 5]]
            , ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantTuple [ConstantBool True, ConstantInt 4]]
            )
            [ ( "x_1_1", DomainBool   , ConstantBool False )
            , ( "x_1_2", intDomain 1 3, ConstantInt 2      )
            , ( "x_2_1", DomainBool   , ConstantBool True  )
            , ( "x_2_2", intDomain 2 5, ConstantInt 4      )
            ]
        , testCase "up1" $ up1Test
            ( "x", DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainTuple [DomainBool, intDomain 2 5]] )
            [ ( "x_1", ConstantTuple [ConstantBool False, ConstantInt 2] )
            , ( "x_2", ConstantTuple [ConstantBool True , ConstantInt 4] )
            ]
            ( "x", ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantTuple [ConstantBool True, ConstantInt 4]] )
        , testCase "up" $ upTest
            ( "x", DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainTuple [DomainBool, intDomain 2 5]] )
            [ ( "x_1_1", ConstantBool False )
            , ( "x_1_2", ConstantInt 2      )
            , ( "x_2_1", ConstantBool True  )
            , ( "x_2_2", ConstantInt 4      )
            ]
            ( "x", ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantTuple [ConstantBool True, ConstantInt 4]] )
        ]

    , testGroup "(bool, (int, (bool, int)))"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]]]
            , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantTuple [ConstantBool True, ConstantInt 4]]]
            )
            (Just [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]]
                           , ConstantTuple [ConstantInt 2, ConstantTuple [ConstantBool True, ConstantInt 4]] )
                  ])
        , testCase "down" $ downTest
            ( "x"
            , DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]]]
            , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantTuple [ConstantBool True, ConstantInt 4]]]
            )
            [ ( "x_1"    , DomainBool   , ConstantBool False )
            , ( "x_2_1"  , intDomain 1 3, ConstantInt 2      )
            , ( "x_2_2_1", DomainBool   , ConstantBool True  )
            , ( "x_2_2_2", intDomain 2 5, ConstantInt 4      )
            ]
        , testCase "up1" $ up1Test
            ( "x", DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]]] )
            [ ( "x_1", ConstantBool False )
            , ( "x_2", ConstantTuple [ConstantInt 2, ConstantTuple [ConstantBool True, ConstantInt 4]] )
            ]
            ( "x", ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantTuple [ConstantBool True, ConstantInt 4]]] )
        , testCase "up" $ upTest
            ( "x", DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]]] )
            [ ( "x_1"    , ConstantBool False )
            , ( "x_2_1"  , ConstantInt 2      )
            , ( "x_2_2_1", ConstantBool True  )
            , ( "x_2_2_2", ConstantInt 4      )
            ]
            ( "x", ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantTuple [ConstantBool True, ConstantInt 4]]] )
        ]

    , testGroup "(bool, (int, bool), int)"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool], intDomain 2 5]
            , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantBool True], ConstantInt 4]
            )
            (Just [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", DomainTuple [intDomain 1 3, DomainBool], ConstantTuple [ConstantInt 2, ConstantBool True] )
                  , ( "x_3", intDomain 2 5, ConstantInt 4      )
                  ])
        , testCase "down" $ downTest
            ( "x"
            , DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool], intDomain 2 5]
            , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantBool True], ConstantInt 4]
            )
            [ ( "x_1"  , DomainBool   , ConstantBool False )
            , ( "x_2_1", intDomain 1 3, ConstantInt 2      )
            , ( "x_2_2", DomainBool   , ConstantBool True  )
            , ( "x_3"  , intDomain 2 5, ConstantInt 4      )
            ]
        , testCase "up1" $ up1Test
            ( "x", DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool], intDomain 2 5] )
            [ ( "x_1", ConstantBool False )
            , ( "x_2", ConstantTuple [ConstantInt 2, ConstantBool True] )
            , ( "x_3", ConstantInt 4      )
            ]
            ( "x", ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantBool True], ConstantInt 4] )
        , testCase "up" $ upTest
            ( "x", DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool], intDomain 2 5] )
            [ ( "x_1"  , ConstantBool False )
            , ( "x_2_1", ConstantInt 2      )
            , ( "x_2_2", ConstantBool True  )
            , ( "x_3"  , ConstantInt 4      )
            ]
            ( "x", ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantBool True], ConstantInt 4] )
        ]

    , testGroup "(((bool, int), bool), int)"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainTuple [DomainTuple [ DomainTuple [DomainBool, intDomain 1 3], DomainBool], intDomain 2 5]
            , ConstantTuple [ConstantTuple [ ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True], ConstantInt 4]
            )
            (Just [ ( "x_1", DomainTuple [ DomainTuple [DomainBool, intDomain 1 3], DomainBool]
                           , ConstantTuple [ ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True] )
                  , ( "x_2", intDomain 2 5, ConstantInt 4      )
                  ])
        , testCase "down" $ downTest
            ( "x"
            , DomainTuple [DomainTuple [ DomainTuple [DomainBool, intDomain 1 3], DomainBool], intDomain 2 5]
            , ConstantTuple [ConstantTuple [ ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True], ConstantInt 4]
            )
            [ ( "x_1_1_1", DomainBool   , ConstantBool False )
            , ( "x_1_1_2", intDomain 1 3, ConstantInt 2      )
            , ( "x_1_2"  , DomainBool   , ConstantBool True  )
            , ( "x_2"    , intDomain 2 5, ConstantInt 4      )
            ]
        , testCase "up1" $ up1Test
            ( "x", DomainTuple [DomainTuple [ DomainTuple [DomainBool, intDomain 1 3], DomainBool], intDomain 2 5] )
            [ ( "x_1", ConstantTuple [ ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True] )
            , ( "x_2", ConstantInt 4 )
            ]
            ( "x", ConstantTuple [ConstantTuple [ ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True], ConstantInt 4] )
        , testCase "up" $ upTest
            ( "x", DomainTuple [DomainTuple [ DomainTuple [DomainBool, intDomain 1 3], DomainBool], intDomain 2 5] )
            [ ( "x_1_1_1", ConstantBool False )
            , ( "x_1_1_2", ConstantInt 2      )
            , ( "x_1_2"  , ConstantBool True  )
            , ( "x_2"    , ConstantInt 4      )
            ]
            ( "x", ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True], ConstantInt 4] )
        ]

    , testGroup "matrix of (bool, int)"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 0 9])
            , ConstantMatrix (intDomain 1 3)
                [ ConstantTuple [ConstantBool False, ConstantInt 0]
                , ConstantTuple [ConstantBool True , ConstantInt 3]
                , ConstantTuple [ConstantBool False, ConstantInt 4]
                ]
            )
            (Just [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool
                           , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool True, ConstantBool False] )
                  , ( "x_2", DomainMatrix (intDomain 1 3) (intDomain 0 9)
                           , ConstantMatrix (intDomain 1 3) [ConstantInt 0, ConstantInt 3, ConstantInt 4] )
                  ])
        , testCase "down" $ downTest
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 0 9])
            , ConstantMatrix (intDomain 1 3)
                [ ConstantTuple [ConstantBool False, ConstantInt 0]
                , ConstantTuple [ConstantBool True , ConstantInt 3]
                , ConstantTuple [ConstantBool False, ConstantInt 4]
                ]
            )
            [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool
                     , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool True, ConstantBool False] )
            , ( "x_2", DomainMatrix (intDomain 1 3) (intDomain 0 9)
                     , ConstantMatrix (intDomain 1 3) [ConstantInt 0, ConstantInt 3, ConstantInt 4] )
            ]
        , testCase "up1" $ up1Test
            ( "x", DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 0 9]) )
            [ ( "x_1", ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool True, ConstantBool False] )
            , ( "x_2", ConstantMatrix (intDomain 1 3) [ConstantInt 0, ConstantInt 3, ConstantInt 4] )
            ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantBool False, ConstantInt 0]
                    , ConstantTuple [ConstantBool True , ConstantInt 3]
                    , ConstantTuple [ConstantBool False, ConstantInt 4]
                    ] )
        , testCase "up" $ upTest
            ( "x", DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 0 9]) )
            [ ( "x_1", ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool True, ConstantBool False] )
            , ( "x_2", ConstantMatrix (intDomain 1 3) [ConstantInt 0, ConstantInt 3, ConstantInt 4] )
            ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantBool False, ConstantInt 0]
                    , ConstantTuple [ConstantBool True , ConstantInt 3]
                    , ConstantTuple [ConstantBool False, ConstantInt 4]
                    ] )
        ]

    , testGroup "matrix of (bool, int, bool)"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3, DomainBool])
            , ConstantMatrix (intDomain 1 3)
                [ ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True]
                , ConstantTuple [ConstantBool False, ConstantInt 3, ConstantBool False]
                , ConstantTuple [ConstantBool False, ConstantInt 4, ConstantBool False]
                ]
            )
            (Just [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool False] )
                  , ( "x_2", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
                  , ( "x_3", DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
                  ])

        , testCase "down" $ downTest
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3, DomainBool])
            , ConstantMatrix (intDomain 1 3)
                [ ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True]
                , ConstantTuple [ConstantBool False, ConstantInt 3, ConstantBool False]
                , ConstantTuple [ConstantBool True , ConstantInt 4, ConstantBool False]
                ]
            )
            [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
            , ( "x_2", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
            , ( "x_3", DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
            ]

        , testCase "up1" $ up1Test
            ( "x", DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3, DomainBool]) )
            [ ( "x_1", ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
            , ( "x_2", ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
            , ( "x_3", ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
            ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True]
                    , ConstantTuple [ConstantBool False, ConstantInt 3, ConstantBool False]
                    , ConstantTuple [ConstantBool True , ConstantInt 4, ConstantBool False]
                    ] )

        , testCase "up" $ upTest
            ( "x", DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3, DomainBool]) )
            [ ( "x_1", ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
            , ( "x_2", ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
            , ( "x_3", ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
            ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True]
                    , ConstantTuple [ConstantBool False, ConstantInt 3, ConstantBool False]
                    , ConstantTuple [ConstantBool True , ConstantInt 4, ConstantBool False]
                    ] )
        ]

    , testGroup "matrix of ((bool, int), bool)"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool])
            , ConstantMatrix (intDomain 1 3)
                [ ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True]
                , ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantBool False]
                , ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantBool False]
                ]
            )
            (Just [ ( "x_1", DomainMatrix   (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3])
                           , ConstantMatrix (intDomain 1 3)
                                [ ConstantTuple [ConstantBool False, ConstantInt 2]
                                , ConstantTuple [ConstantBool False, ConstantInt 3]
                                , ConstantTuple [ConstantBool True , ConstantInt 4]
                                ] )
                  , ( "x_2", DomainMatrix   (intDomain 1 3) DomainBool
                           , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
                  ])
        , testCase "down" $ downTest
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool])
            , ConstantMatrix (intDomain 1 3)
                [ ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True]
                , ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantBool False]
                , ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantBool False]
                ]
            )
            [ ( "x_1_1", DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
            , ( "x_1_2", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
            , ( "x_2"  , DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
            ]
        , testCase "up1" $ up1Test
            ( "x", DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool]) )
            [ ( "x_1", ConstantMatrix (intDomain 1 3)
                            [ ConstantTuple [ConstantBool False, ConstantInt 2]
                            , ConstantTuple [ConstantBool False, ConstantInt 3]
                            , ConstantTuple [ConstantBool True , ConstantInt 4]
                            ] )
            , ( "x_2", ConstantMatrix (intDomain 1 3)
                            [ ConstantBool True
                            , ConstantBool False
                            , ConstantBool False
                            ] )
            ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True]
                    , ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantBool False]
                    , ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantBool False]
                    ] )

        , testCase "up" $ upTest
            ( "x", DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool]) )
            [ ( "x_1_1", ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
            , ( "x_1_2", ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
            , ( "x_2"  , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
            ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True]
                    , ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantBool False]
                    , ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantBool False]
                    ] )
        ]

    , testGroup "matrix of (bool, (int, bool))"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 0 9, DomainBool]])
            , ConstantMatrix (intDomain 1 3)
                [ ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 0, ConstantBool True]]
                , ConstantTuple [ConstantBool True , ConstantTuple [ConstantInt 3, ConstantBool False]]
                , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 4, ConstantBool True]]
                ]
            )
            (Just [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool
                           , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool True, ConstantBool False] )
                  , ( "x_2", DomainMatrix (intDomain 1 3) (DomainTuple [intDomain 0 9, DomainBool])
                           , ConstantMatrix (intDomain 1 3)
                               [ ConstantTuple [ConstantInt 0, ConstantBool True]
                               , ConstantTuple [ConstantInt 3, ConstantBool False]
                               , ConstantTuple [ConstantInt 4, ConstantBool True]
                               ] )
                  ])
        , testCase "down" $ downTest
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 0 9, DomainBool]])
            , ConstantMatrix (intDomain 1 3)
                [ ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 0, ConstantBool True]]
                , ConstantTuple [ConstantBool True , ConstantTuple [ConstantInt 3, ConstantBool False]]
                , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 4, ConstantBool True]]
                ]
            )
            [ ( "x_1"  , DomainMatrix (intDomain 1 3) DomainBool
                       , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool True, ConstantBool False] )
            , ( "x_2_1", DomainMatrix (intDomain 1 3) (intDomain 0 9)
                       , ConstantMatrix (intDomain 1 3) [ConstantInt 0, ConstantInt 3, ConstantInt 4] )
            , ( "x_2_2", DomainMatrix (intDomain 1 3) DomainBool
                       , ConstantMatrix (intDomain 1 3) [ConstantBool True, ConstantBool False, ConstantBool True] )
            ]
        , testCase "up1" $ up1Test
            ( "x", DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 0 9, DomainBool]]) )
            [ ( "x_1", ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool True, ConstantBool False] )
            , ( "x_2", ConstantMatrix (intDomain 1 3)
                           [ ConstantTuple [ConstantInt 0, ConstantBool True]
                           , ConstantTuple [ConstantInt 3, ConstantBool False]
                           , ConstantTuple [ConstantInt 4, ConstantBool True]
                           ] )
            ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 0, ConstantBool True]]
                    , ConstantTuple [ConstantBool True , ConstantTuple [ConstantInt 3, ConstantBool False]]
                    , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 4, ConstantBool True]]
                    ] )
        , testCase "up" $ upTest
            ( "x", DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 0 9, DomainBool]]) )
            [ ( "x_1"  , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool True, ConstantBool False] )
            , ( "x_2_1", ConstantMatrix (intDomain 1 3) [ConstantInt 0, ConstantInt 3, ConstantInt 4] )
            , ( "x_2_2", ConstantMatrix (intDomain 1 3) [ConstantBool True, ConstantBool False, ConstantBool True] )
            ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 0, ConstantBool True]]
                    , ConstantTuple [ConstantBool True , ConstantTuple [ConstantInt 3, ConstantBool False]]
                    , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 4, ConstantBool True]]
                    ] )
        ]

    , testGroup "matrix of (bool, int, bool, int)"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3, DomainBool, intDomain 2 5])
            , ConstantMatrix (intDomain 1 3)
                [ ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True , ConstantInt 4]
                , ConstantTuple [ConstantBool False, ConstantInt 3, ConstantBool False, ConstantInt 6]
                , ConstantTuple [ConstantBool True , ConstantInt 4, ConstantBool False, ConstantInt 8]
                ]
            )
            (Just [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
                  , ( "x_2", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
                  , ( "x_3", DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
                  , ( "x_4", DomainMatrix (intDomain 1 3) (intDomain 2 5), ConstantMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ] )
                  ])
        , testCase "down" $ downTest
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3, DomainBool, intDomain 2 5])
            , ConstantMatrix (intDomain 1 3)
                [ ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True , ConstantInt 4]
                , ConstantTuple [ConstantBool False, ConstantInt 3, ConstantBool False, ConstantInt 6]
                , ConstantTuple [ConstantBool True , ConstantInt 4, ConstantBool False, ConstantInt 8]
                ]
            )
            [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
            , ( "x_2", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
            , ( "x_3", DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
            , ( "x_4", DomainMatrix (intDomain 1 3) (intDomain 2 5), ConstantMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ] )
            ]
        , testCase "up1" $ up1Test
            ( "x", DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3, DomainBool, intDomain 2 5]) )
            [ ( "x_1", ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
            , ( "x_2", ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
            , ( "x_3", ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
            , ( "x_4", ConstantMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ] )
            ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True , ConstantInt 4]
                    , ConstantTuple [ConstantBool False, ConstantInt 3, ConstantBool False, ConstantInt 6]
                    , ConstantTuple [ConstantBool True , ConstantInt 4, ConstantBool False, ConstantInt 8]
                    ] )
        , testCase "up" $ upTest
            ( "x", DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3, DomainBool, intDomain 2 5]) )
            [ ( "x_1", ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
            , ( "x_2", ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
            , ( "x_3", ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
            , ( "x_4", ConstantMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ] )
            ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True , ConstantInt 4]
                    , ConstantTuple [ConstantBool False, ConstantInt 3, ConstantBool False, ConstantInt 6]
                    , ConstantTuple [ConstantBool True , ConstantInt 4, ConstantBool False, ConstantInt 8]
                    ] )
        ]

    , testGroup "matrix of ((bool, int), (bool, int))"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainTuple [DomainBool, intDomain 2 5]])
            , ConstantMatrix (intDomain 1 3)
                [ ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantTuple [ConstantBool True , ConstantInt 4]]
                , ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantTuple [ConstantBool False, ConstantInt 6]]
                , ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantTuple [ConstantBool False, ConstantInt 8]]
                ]
            )
            (Just [ ( "x_1"
                    , DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3])
                    , ConstantMatrix (intDomain 1 3)
                        [ ConstantTuple [ConstantBool False, ConstantInt 2]
                        , ConstantTuple [ConstantBool False, ConstantInt 3]
                        , ConstantTuple [ConstantBool True , ConstantInt 4]
                        ] )
                  , ( "x_2"
                    , DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 2 5])
                    , ConstantMatrix (intDomain 1 3)
                        [ ConstantTuple [ConstantBool True , ConstantInt 4]
                        , ConstantTuple [ConstantBool False, ConstantInt 6]
                        , ConstantTuple [ConstantBool False, ConstantInt 8]
                        ] )
                  ])
        , testCase "down" $ downTest
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainTuple [DomainBool, intDomain 2 5]])
            , ConstantMatrix (intDomain 1 3)
                [ ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantTuple [ConstantBool True , ConstantInt 4]]
                , ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantTuple [ConstantBool False, ConstantInt 6]]
                , ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantTuple [ConstantBool False, ConstantInt 8]]
                ]
            )
            [ ( "x_1_1", DomainMatrix   (intDomain 1 3) DomainBool
                       , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
            , ( "x_1_2", DomainMatrix   (intDomain 1 3) (intDomain 1 3)
                       , ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
            , ( "x_2_1", DomainMatrix   (intDomain 1 3) DomainBool
                       , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
            , ( "x_2_2", DomainMatrix   (intDomain 1 3) (intDomain 2 5)
                       , ConstantMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ] )
            ]
        , testCase "up1" $ up1Test
            ( "x", DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainTuple [DomainBool, intDomain 2 5]]) )
            [ ( "x_1", ConstantMatrix (intDomain 1 3)
                        [ ConstantTuple [ConstantBool False, ConstantInt 2]
                        , ConstantTuple [ConstantBool False, ConstantInt 3]
                        , ConstantTuple [ConstantBool True , ConstantInt 4]
                        ] )
            , ( "x_2", ConstantMatrix (intDomain 1 3)
                        [ ConstantTuple [ConstantBool True , ConstantInt 4]
                        , ConstantTuple [ConstantBool False, ConstantInt 6]
                        , ConstantTuple [ConstantBool False, ConstantInt 8]
                        ] )
            ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantTuple [ConstantBool True , ConstantInt 4]]
                    , ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantTuple [ConstantBool False, ConstantInt 6]]
                    , ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantTuple [ConstantBool False, ConstantInt 8]]
                    ] )
        , testCase "up" $ upTest
            ( "x", DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainTuple [DomainBool, intDomain 2 5]]) )
            [ ( "x_1_1", ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
            , ( "x_1_2", ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
            , ( "x_2_1", ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
            , ( "x_2_2", ConstantMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ] )
            ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantTuple [ConstantBool True , ConstantInt 4]]
                    , ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantTuple [ConstantBool False, ConstantInt 6]]
                    , ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantTuple [ConstantBool False, ConstantInt 8]]
                    ] )
        ]

    , testGroup "matrix of (bool, (int, (bool, int)))"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]]])
            , ConstantMatrix (intDomain 1 3)
                [ ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantTuple [ConstantBool True , ConstantInt 4]]]
                , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 3, ConstantTuple [ConstantBool False, ConstantInt 6]]]
                , ConstantTuple [ConstantBool True , ConstantTuple [ConstantInt 4, ConstantTuple [ConstantBool False, ConstantInt 8]]]
                ]
            )
            (Just [ ( "x_1", DomainMatrix   (intDomain 1 3) DomainBool
                           , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True] )
                  , ( "x_2", DomainMatrix   (intDomain 1 3) (DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]])
                           , ConstantMatrix (intDomain 1 3)
                               [ ConstantTuple [ConstantInt 2, ConstantTuple [ConstantBool True , ConstantInt 4]]
                               , ConstantTuple [ConstantInt 3, ConstantTuple [ConstantBool False, ConstantInt 6]]
                               , ConstantTuple [ConstantInt 4, ConstantTuple [ConstantBool False, ConstantInt 8]]
                               ] )
                  ])
        , testCase "down" $ downTest
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]]])
            , ConstantMatrix (intDomain 1 3)
                [ ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantTuple [ConstantBool True , ConstantInt 4]]]
                , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 3, ConstantTuple [ConstantBool False, ConstantInt 6]]]
                , ConstantTuple [ConstantBool True , ConstantTuple [ConstantInt 4, ConstantTuple [ConstantBool False, ConstantInt 8]]]
                ]
            )
            [ ( "x_1"    , DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ])
            , ( "x_2_1"  , DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ])
            , ( "x_2_2_1", DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False])
            , ( "x_2_2_2", DomainMatrix (intDomain 1 3) (intDomain 2 5), ConstantMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ])
            ]
        , testCase "up1" $ up1Test
            ( "x", DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]]]) )
            [ ( "x_1", ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True] )
            , ( "x_2", ConstantMatrix (intDomain 1 3)
                        [ ConstantTuple [ConstantInt 2, ConstantTuple [ConstantBool True , ConstantInt 4]]
                        , ConstantTuple [ConstantInt 3, ConstantTuple [ConstantBool False, ConstantInt 6]]
                        , ConstantTuple [ConstantInt 4, ConstantTuple [ConstantBool False, ConstantInt 8]]
                        ] )
            ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantTuple [ConstantBool True , ConstantInt 4]]]
                    , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 3, ConstantTuple [ConstantBool False, ConstantInt 6]]]
                    , ConstantTuple [ConstantBool True , ConstantTuple [ConstantInt 4, ConstantTuple [ConstantBool False, ConstantInt 8]]]
                    ] )
        , testCase "up" $ upTest
            ( "x", DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]]]) )
            [ ( "x_1"    , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ])
            , ( "x_2_1"  , ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ])
            , ( "x_2_2_1", ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False])
            , ( "x_2_2_2", ConstantMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ])
            ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantTuple [ConstantBool True , ConstantInt 4]]]
                    , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 3, ConstantTuple [ConstantBool False, ConstantInt 6]]]
                    , ConstantTuple [ConstantBool True , ConstantTuple [ConstantInt 4, ConstantTuple [ConstantBool False, ConstantInt 8]]]
                    ] )
        ]

    , testGroup "matrix of (bool, (int, bool), int)"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool], intDomain 2 5])
            , ConstantMatrix (intDomain 1 3)
                [ ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantBool True ], ConstantInt 4]
                , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 3, ConstantBool False], ConstantInt 6]
                , ConstantTuple [ConstantBool True , ConstantTuple [ConstantInt 4, ConstantBool False], ConstantInt 8]
                ]
            )
            (Just [ ( "x_1", DomainMatrix   (intDomain 1 3) DomainBool
                           , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True] )
                  , ( "x_2", DomainMatrix   (intDomain 1 3) (DomainTuple [intDomain 1 3, DomainBool])
                           , ConstantMatrix (intDomain 1 3)
                               [ ConstantTuple [ConstantInt 2, ConstantBool True ]
                               , ConstantTuple [ConstantInt 3, ConstantBool False]
                               , ConstantTuple [ConstantInt 4, ConstantBool False]
                               ] )
                  , ( "x_3", DomainMatrix   (intDomain 1 3) (intDomain 2 5)
                           , ConstantMatrix (intDomain 1 3) [ConstantInt 4, ConstantInt 6, ConstantInt 8]
                           )
                  ])
        , testCase "down" $ downTest
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool], intDomain 2 5])
            , ConstantMatrix (intDomain 1 3)
                [ ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantBool True ], ConstantInt 4]
                , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 3, ConstantBool False], ConstantInt 6]
                , ConstantTuple [ConstantBool True , ConstantTuple [ConstantInt 4, ConstantBool False], ConstantInt 8]
                ]
            )
            [ ( "x_1"  , DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ])
            , ( "x_2_1", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ])
            , ( "x_2_2", DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False])
            , ( "x_3"  , DomainMatrix (intDomain 1 3) (intDomain 2 5), ConstantMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ])
            ]
        , testCase "up1" $ up1Test
            ( "x", DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool], intDomain 2 5]) )
            [ ( "x_1", ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True] )
            , ( "x_2", ConstantMatrix (intDomain 1 3)
                        [ ConstantTuple [ConstantInt 2, ConstantBool True ]
                        , ConstantTuple [ConstantInt 3, ConstantBool False]
                        , ConstantTuple [ConstantInt 4, ConstantBool False]
                        ] )
            , ( "x_3", ConstantMatrix (intDomain 1 3) [ConstantInt 4, ConstantInt 6, ConstantInt 8] )
            ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantBool True ], ConstantInt 4]
                    , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 3, ConstantBool False], ConstantInt 6]
                    , ConstantTuple [ConstantBool True , ConstantTuple [ConstantInt 4, ConstantBool False], ConstantInt 8]
                    ] )
        , testCase "up" $ upTest
            ( "x", DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool], intDomain 2 5]) )
            [ ( "x_1"  , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ])
            , ( "x_2_1", ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ])
            , ( "x_2_2", ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False])
            , ( "x_3"  , ConstantMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ])
            ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantBool True ], ConstantInt 4]
                    , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 3, ConstantBool False], ConstantInt 6]
                    , ConstantTuple [ConstantBool True , ConstantTuple [ConstantInt 4, ConstantBool False], ConstantInt 8]
                    ] )
        ]

    , testGroup "matrix of (((bool, int), bool), int)"
        [ testCase "down1" $ down1Test
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool], intDomain 2 5])
            , ConstantMatrix (intDomain 1 3)
                [ ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True ], ConstantInt 4]
                , ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantBool False], ConstantInt 6]
                , ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantBool False], ConstantInt 8]
                ]
            )
            (Just [ ( "x_1", DomainMatrix   (intDomain 1 3) (DomainTuple [DomainTuple [DomainBool,intDomain 1 3],DomainBool])
                           , ConstantMatrix (intDomain 1 3)
                               [ ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True ]
                               , ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantBool False]
                               , ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantBool False]
                               ])
                  , ( "x_2", DomainMatrix   (intDomain 1 3) (intDomain 2 5)
                           , ConstantMatrix (intDomain 1 3) [ConstantInt 4, ConstantInt 6, ConstantInt 8]
                           )
                  ])
        , testCase "down" $ downTest
            ( "x"
            , DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool], intDomain 2 5])
            , ConstantMatrix (intDomain 1 3)
                [ ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True ], ConstantInt 4]
                , ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantBool False], ConstantInt 6]
                , ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantBool False], ConstantInt 8]
                ]
            )
            [ ( "x_1_1_1", DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ])
            , ( "x_1_1_2", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ])
            , ( "x_1_2"  , DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False])
            , ( "x_2"    , DomainMatrix (intDomain 1 3) (intDomain 2 5), ConstantMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ])
            ]
        , testCase "up1" $ up1Test
            ( "x", DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool], intDomain 2 5]) )
            [ ( "x_1", ConstantMatrix (intDomain 1 3)
                        [ ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True ]
                        , ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantBool False]
                        , ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantBool False]
                        ])
            , ( "x_2", ConstantMatrix (intDomain 1 3) [ConstantInt 4, ConstantInt 6, ConstantInt 8] )
            ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True ], ConstantInt 4]
                    , ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantBool False], ConstantInt 6]
                    , ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantBool False], ConstantInt 8]
                    ] )
        , testCase "up" $ upTest
            ( "x", DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool], intDomain 2 5]) )
            [ ( "x_1_1_1", ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ])
            , ( "x_1_1_2", ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ])
            , ( "x_1_2"  , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False])
            , ( "x_2"    , ConstantMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ])
            ]
            ( "x", ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True ], ConstantInt 4]
                    , ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantBool False], ConstantInt 6]
                    , ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantBool False], ConstantInt 8]
                    ] )
        ]

    , testGroup "matrix 2d of (((bool, int), bool), int)" $
        let
            highDomain =
                DomainMatrix (intDomain 1 2)
                    (DomainMatrix (intDomain 1 3)
                        (DomainTuple [DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool], intDomain 2 5]))
            highConstant =
                ConstantMatrix (intDomain 1 2)
                    [ ConstantMatrix (intDomain 1 3)
                        [ ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True ], ConstantInt 4]
                        , ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantBool False], ConstantInt 6]
                        , ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantBool False], ConstantInt 8]
                        ]
                    , ConstantMatrix (intDomain 1 3)
                        [ ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 4], ConstantBool True ], ConstantInt 4]
                        , ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 5], ConstantBool False], ConstantInt 7]
                        , ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 6], ConstantBool False], ConstantInt 9]
                        ]
                    ]
            mid =
                [ ( "x_1" , DomainMatrix (intDomain 1 2)
                              (DomainMatrix (intDomain 1 3)
                                  (DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool]))
                          , ConstantMatrix (intDomain 1 2)
                              [ ConstantMatrix (intDomain 1 3)
                                  [ ConstantTuple [ConstantTuple [ConstantBool False,ConstantInt 2],ConstantBool True]
                                  , ConstantTuple [ConstantTuple [ConstantBool False,ConstantInt 3],ConstantBool False]
                                  , ConstantTuple [ConstantTuple [ConstantBool True,ConstantInt 4],ConstantBool False]
                                  ]
                              , ConstantMatrix (intDomain 1 3)
                                  [ ConstantTuple [ConstantTuple [ConstantBool False,ConstantInt 4],ConstantBool True]
                                  , ConstantTuple [ConstantTuple [ConstantBool True,ConstantInt 5],ConstantBool False]
                                  , ConstantTuple [ConstantTuple [ConstantBool True,ConstantInt 6],ConstantBool False]
                                  ]
                              ] )
                , ( "x_2" , DomainMatrix (intDomain 1 2)
                                  (DomainMatrix (intDomain 1 3)
                                      (intDomain 2 5))
                          , ConstantMatrix (intDomain 1 2)
                                [ ConstantMatrix (intDomain 1 3) [ConstantInt 4,ConstantInt 6,ConstantInt 8]
                                , ConstantMatrix (intDomain 1 3) [ConstantInt 4,ConstantInt 7,ConstantInt 9]
                                ] )
                ]
            low =
                [ ( "x_1_1_1" , DomainMatrix   (intDomain 1 2) (DomainMatrix (intDomain 1 3) DomainBool)
                              , ConstantMatrix (intDomain 1 2)
                                  [ ConstantMatrix (intDomain 1 3) [ConstantBool False,ConstantBool False,ConstantBool True]
                                  , ConstantMatrix (intDomain 1 3) [ConstantBool False,ConstantBool True,ConstantBool True]
                                  ] )
                , ( "x_1_1_2" , DomainMatrix   (intDomain 1 2) (DomainMatrix (intDomain 1 3) (intDomain 1 3))
                              , ConstantMatrix (intDomain 1 2)
                                  [ ConstantMatrix (intDomain 1 3) [ConstantInt 2,ConstantInt 3,ConstantInt 4]
                                  , ConstantMatrix (intDomain 1 3) [ConstantInt 4,ConstantInt 5,ConstantInt 6]
                                  ] )
                , ( "x_1_2"   , DomainMatrix   (intDomain 1 2) (DomainMatrix (intDomain 1 3) DomainBool)
                              , ConstantMatrix (intDomain 1 2)
                                  [ ConstantMatrix (intDomain 1 3) [ConstantBool True,ConstantBool False,ConstantBool False]
                                  , ConstantMatrix (intDomain 1 3) [ConstantBool True,ConstantBool False,ConstantBool False]
                                  ] )
                , ( "x_2"     , DomainMatrix   (intDomain 1 2) (DomainMatrix (intDomain 1 3) (intDomain 2 5))
                              , ConstantMatrix (intDomain 1 2)
                                  [ ConstantMatrix (intDomain 1 3) [ConstantInt 4,ConstantInt 6,ConstantInt 8]
                                  , ConstantMatrix (intDomain 1 3) [ConstantInt 4,ConstantInt 7,ConstantInt 9]
                                  ] )
                ]
        in testCases "x" highDomain highConstant Just mid low

    ]


testCases
    :: Text                                                                                     -- high level variable name
    -> Domain Representation Constant                                                           -- high level domain
    -> Constant                                                                                 -- high level value (constant)
    -> ([(Text, a, Constant)] -> Maybe [(Text, Domain Representation Constant, Constant)])      -- `const Nothing` -- if going one level down produces Nothing
                                                                                                -- `Just`          -- if going one level down produces (Just mid)
    ->  [(Text, a, Constant)]                                                                   -- "mid" result, if we go one level down
    -> [(Text, Domain Representation Constant, Constant)]                                       -- "low" result, if we go all the way down
    -> [TestTree]
testCases highName highDomain highConstant mkMid mid low =
    [ testCase "down1" $ down1Test (highName, highDomain, highConstant) (mkMid mid)
    , testCase "down"  $ downTest  (highName, highDomain, highConstant) low
    , testCase "up1"   $ up1Test   (highName, highDomain) (dropDomain mid) ("x", highConstant)
    , testCase "up"    $ upTest    (highName, highDomain) (dropDomain low) ("x", highConstant)
    ]

down1Test
    :: (Text, Domain Representation Constant, Constant)
    -> Maybe [(Text, Domain Representation Constant, Constant)]
    -> Assertion
down1Test high low' =
    case down1_ dispatch high of
        Left err -> assertFailure (show err)
        Right low -> Pr low @?= Pr low'

downTest
    :: (Text, Domain Representation Constant, Constant)
    -> [(Text, Domain Representation Constant, Constant)]
    -> Assertion
downTest high lows' =
    case down_ high of
        Left err -> assertFailure (show err)
        Right lows -> Pr lows @?= Pr lows'

up1Test
    :: (Text, Domain Representation Constant)
    -> [(Text, Constant)]
    -> (Text, Constant)
    -> Assertion
up1Test info lows high' =
    case up1 dispatch info lows of
        Left err -> assertFailure (show err)
        Right high -> Pr high @?= Pr high'

upTest
    :: (Text, Domain Representation Constant)
    -> [(Text, Constant)]
    -> (Text, Constant)
    -> Assertion
upTest info lows high' =
    case up info lows of
        Left err -> assertFailure (show err)
        Right high -> Pr high @?= Pr high'


intDomain :: Int -> Int -> Domain r Constant
intDomain lb ub = DomainInt [RangeBounded (ConstantInt lb) (ConstantInt ub)]

dropDomain :: [(a,b,c)] -> [(a,c)]
dropDomain xs = [ (a,c) | (a,_,c) <- xs ]


data Pr a = Pr a
    deriving Eq

instance Show (Pr [(Text, Domain Representation Constant, Constant)]) where
    show (Pr xs) = show $ vcat $ concatMap sh xs
        where
            sh (name, dom, cons) = [ hang (pretty name) 4 $ vcat
                                        [ ":" <+> pretty dom
                                        , "=" <+> pretty cons
                                        ]
                                   ]

instance Show (Pr (Maybe [(Text, Domain Representation Constant, Constant)])) where
    show (Pr Nothing) = "Nothing"
    show (Pr (Just xs)) = show (Pr xs)

instance Show (Pr (Text, Constant)) where
    show (Pr (name, cons)) = show $ pretty name <+> "=" <+> pretty cons

instance Show (Pr [(Text, Constant)]) where
    show (Pr xs) = intercalate "\n" $ map (show . Pr) xs


