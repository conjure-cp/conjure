{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

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

    [ testGroup "bool #1" $
        let
            highDomain = DomainBool
            highConstant = ConstantBool False
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testGroup "bool #2" $
        let
            highDomain = DomainBool
            highConstant = ConstantBool True
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testGroup "int #1" $
        let
            highDomain = DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 4)]
            highConstant = ConstantInt 3
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testGroup "matrix of bool" $
        let
            highDomain = DomainMatrix (intDomain 1 3) DomainBool
            highConstant = ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True]
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testGroup "matrix of int" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (intDomain 1 5)
            highConstant = ConstantMatrix (intDomain 1 3) [ConstantInt 2, ConstantInt 3, ConstantInt 5]
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testGroup "matrix 2d of bool" $
        let
            highDomain =
                DomainMatrix (intDomain 1 3) (DomainMatrix (intDomain 1 2) DomainBool)
            highConstant =
                ConstantMatrix (intDomain 1 3)
                    [ ConstantMatrix (intDomain 1 2) [ConstantBool False, ConstantBool True ]
                    , ConstantMatrix (intDomain 1 2) [ConstantBool True , ConstantBool False]
                    , ConstantMatrix (intDomain 1 2) [ConstantBool True , ConstantBool True ]
                    ]
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testGroup "matrix 2d of int" $
        let
            highDomain =
                DomainMatrix (intDomain 1 3) (DomainMatrix (intDomain 1 2) (intDomain 0 9))
            highConstant =
                ConstantMatrix (intDomain 1 3)
                    [ ConstantMatrix (intDomain 1 2) [ConstantInt 3, ConstantInt 7]
                    , ConstantMatrix (intDomain 1 2) [ConstantInt 2, ConstantInt 8]
                    , ConstantMatrix (intDomain 1 2) [ConstantInt 0, ConstantInt 1]
                    ]
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testGroup "(bool, int)" $
        let
            highDomain = DomainTuple [DomainBool, intDomain 1 3]
            highConstant = ConstantTuple [ConstantBool False, ConstantInt 2]
            low = [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", intDomain 1 3, ConstantInt 2      )
                  ]
        in  testCases "x" highDomain highConstant Just low low

    , testGroup "(bool, int, bool)" $
        let
            highDomain = DomainTuple [DomainBool, intDomain 1 3, DomainBool]
            highConstant = ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True]
            low = [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", intDomain 1 3, ConstantInt 2      )
                  , ( "x_3", DomainBool   , ConstantBool True  )
                  ]
        in  testCases "x" highDomain highConstant Just low low

    , testGroup "((bool, int), bool)" $
        let
            highDomain = DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool]
            highConstant = ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True]
            mid = [ ( "x_1", DomainTuple [DomainBool, intDomain 1 3], ConstantTuple [ConstantBool False, ConstantInt 2] )
                  , ( "x_2", DomainBool, ConstantBool True )
                  ]
            low = [ ( "x_1_1", DomainBool   , ConstantBool False )
                  , ( "x_1_2", intDomain 1 3, ConstantInt 2      )
                  , ( "x_2"  , DomainBool   , ConstantBool True  )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "(bool, (int, bool))" $
        let
            highDomain = DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool]]
            highConstant = ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantBool True]]
            mid = [ ( "x_1", DomainBool, ConstantBool False )
                  , ( "x_2", DomainTuple [intDomain 1 3, DomainBool], ConstantTuple [ConstantInt 2, ConstantBool True] )
                  ]
            low = [ ( "x_1"  , DomainBool   , ConstantBool False )
                  , ( "x_2_1", intDomain 1 3, ConstantInt 2      )
                  , ( "x_2_2", DomainBool   , ConstantBool True  )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "(bool, int, bool, int)" $
        let
            highDomain = DomainTuple [DomainBool, intDomain 1 3, DomainBool, intDomain 2 5]
            highConstant = ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True, ConstantInt 4]
            low = [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", intDomain 1 3, ConstantInt 2      )
                  , ( "x_3", DomainBool   , ConstantBool True  )
                  , ( "x_4", intDomain 2 5, ConstantInt 4      )
                  ]
        in  testCases "x" highDomain highConstant Just low low

    , testGroup "((bool, int), (bool, int))" $
        let
            highDomain = DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainTuple [DomainBool, intDomain 2 5]]
            highConstant = ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantTuple [ConstantBool True, ConstantInt 4]]
            mid = [ ( "x_1", DomainTuple [DomainBool, intDomain 1 3], ConstantTuple [ConstantBool False, ConstantInt 2] )
                  , ( "x_2", DomainTuple [DomainBool, intDomain 2 5], ConstantTuple [ConstantBool True , ConstantInt 4] )
                  ]
            low = [ ( "x_1_1", DomainBool   , ConstantBool False )
                  , ( "x_1_2", intDomain 1 3, ConstantInt 2      )
                  , ( "x_2_1", DomainBool   , ConstantBool True  )
                  , ( "x_2_2", intDomain 2 5, ConstantInt 4      )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "(bool, (int, (bool, int)))" $
        let
            highDomain = DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]]]
            highConstant = ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantTuple [ConstantBool True, ConstantInt 4]]]
            mid = [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]]
                           , ConstantTuple [ConstantInt 2, ConstantTuple [ConstantBool True, ConstantInt 4]] )
                  ]
            low = [ ( "x_1"    , DomainBool   , ConstantBool False )
                  , ( "x_2_1"  , intDomain 1 3, ConstantInt 2      )
                  , ( "x_2_2_1", DomainBool   , ConstantBool True  )
                  , ( "x_2_2_2", intDomain 2 5, ConstantInt 4      )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "(bool, (int, bool), int)" $
        let
            highDomain = DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool], intDomain 2 5]
            highConstant = ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantBool True], ConstantInt 4]
            mid = [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", DomainTuple [intDomain 1 3, DomainBool], ConstantTuple [ConstantInt 2, ConstantBool True] )
                  , ( "x_3", intDomain 2 5, ConstantInt 4 )
                  ]
            low = [ ( "x_1"  , DomainBool   , ConstantBool False )
                  , ( "x_2_1", intDomain 1 3, ConstantInt 2      )
                  , ( "x_2_2", DomainBool   , ConstantBool True  )
                  , ( "x_3"  , intDomain 2 5, ConstantInt 4      )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "(((bool, int), bool), int)" $
        let
            highDomain = DomainTuple [DomainTuple [ DomainTuple [DomainBool, intDomain 1 3], DomainBool], intDomain 2 5]
            highConstant = ConstantTuple [ConstantTuple [ ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True], ConstantInt 4]
            mid = [ ( "x_1", DomainTuple [ DomainTuple [DomainBool, intDomain 1 3], DomainBool]
                           , ConstantTuple [ ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True] )
                  , ( "x_2", intDomain 2 5, ConstantInt 4 )
                  ]
            low = [ ( "x_1_1_1", DomainBool   , ConstantBool False )
                  , ( "x_1_1_2", intDomain 1 3, ConstantInt 2      )
                  , ( "x_1_2"  , DomainBool   , ConstantBool True  )
                  , ( "x_2"    , intDomain 2 5, ConstantInt 4      )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "matrix of (bool, int)" $
        let
            highDomain =
                DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 0 9])
            highConstant =
                ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantBool False, ConstantInt 0]
                    , ConstantTuple [ConstantBool True , ConstantInt 3]
                    , ConstantTuple [ConstantBool False, ConstantInt 4]
                    ]
            low = [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool
                           , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool True, ConstantBool False] )
                  , ( "x_2", DomainMatrix (intDomain 1 3) (intDomain 0 9)
                           , ConstantMatrix (intDomain 1 3) [ConstantInt 0, ConstantInt 3, ConstantInt 4] )
                  ]
        in  testCases "x" highDomain highConstant Just low low

    , testGroup "matrix of (bool, int, bool)" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3, DomainBool])
            highConstant =
                ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True]
                    , ConstantTuple [ConstantBool False, ConstantInt 3, ConstantBool False]
                    , ConstantTuple [ConstantBool False, ConstantInt 4, ConstantBool False]
                    ]
            low = [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool False] )
                  , ( "x_2", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
                  , ( "x_3", DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
                  ]
        in  testCases "x" highDomain highConstant Just low low

    , testGroup "matrix of ((bool, int), bool)" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool])
            highConstant = 
                ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True]
                    , ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantBool False]
                    , ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantBool False]
                    ]
            mid = [ ( "x_1", DomainMatrix   (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3])
                           , ConstantMatrix (intDomain 1 3)
                                [ ConstantTuple [ConstantBool False, ConstantInt 2]
                                , ConstantTuple [ConstantBool False, ConstantInt 3]
                                , ConstantTuple [ConstantBool True , ConstantInt 4]
                                ] )
                  , ( "x_2", DomainMatrix   (intDomain 1 3) DomainBool
                           , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
                  ]
            low = [ ( "x_1_1", DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
                  , ( "x_1_2", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
                  , ( "x_2"  , DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "matrix of (bool, (int, bool))" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 0 9, DomainBool]])
            highConstant =
                ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 0, ConstantBool True]]
                    , ConstantTuple [ConstantBool True , ConstantTuple [ConstantInt 3, ConstantBool False]]
                    , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 4, ConstantBool True]]
                    ]
            mid = [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool
                           , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool True, ConstantBool False] )
                  , ( "x_2", DomainMatrix (intDomain 1 3) (DomainTuple [intDomain 0 9, DomainBool])
                           , ConstantMatrix (intDomain 1 3)
                               [ ConstantTuple [ConstantInt 0, ConstantBool True]
                               , ConstantTuple [ConstantInt 3, ConstantBool False]
                               , ConstantTuple [ConstantInt 4, ConstantBool True]
                               ] )
                  ]
            low = [ ( "x_1"  , DomainMatrix (intDomain 1 3) DomainBool
                             , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool True, ConstantBool False] )
                  , ( "x_2_1", DomainMatrix (intDomain 1 3) (intDomain 0 9)
                             , ConstantMatrix (intDomain 1 3) [ConstantInt 0, ConstantInt 3, ConstantInt 4] )
                  , ( "x_2_2", DomainMatrix (intDomain 1 3) DomainBool
                             , ConstantMatrix (intDomain 1 3) [ConstantBool True, ConstantBool False, ConstantBool True] )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "matrix of (bool, int, bool, int)" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3, DomainBool, intDomain 2 5])
            highConstant =
                ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantBool False, ConstantInt 2, ConstantBool True , ConstantInt 4]
                    , ConstantTuple [ConstantBool False, ConstantInt 3, ConstantBool False, ConstantInt 6]
                    , ConstantTuple [ConstantBool True , ConstantInt 4, ConstantBool False, ConstantInt 8]
                    ]
            low = [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
                  , ( "x_2", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
                  , ( "x_3", DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
                  , ( "x_4", DomainMatrix (intDomain 1 3) (intDomain 2 5), ConstantMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ] )
                  ]
        in  testCases "x" highDomain highConstant Just low low

    , testGroup "matrix of ((bool, int), (bool, int))" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainTuple [DomainBool, intDomain 2 5]])
            highConstant =
                ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantTuple [ConstantBool True , ConstantInt 4]]
                    , ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantTuple [ConstantBool False, ConstantInt 6]]
                    , ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantTuple [ConstantBool False, ConstantInt 8]]
                    ]
            mid = [ ( "x_1"
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
                  ]
            low = [ ( "x_1_1", DomainMatrix   (intDomain 1 3) DomainBool
                             , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
                  , ( "x_1_2", DomainMatrix   (intDomain 1 3) (intDomain 1 3)
                             , ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
                  , ( "x_2_1", DomainMatrix   (intDomain 1 3) DomainBool
                             , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
                  , ( "x_2_2", DomainMatrix   (intDomain 1 3) (intDomain 2 5)
                             , ConstantMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ] )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "matrix of (bool, (int, (bool, int)))" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]]])
            highConstant =
                ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantTuple [ConstantBool True , ConstantInt 4]]]
                    , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 3, ConstantTuple [ConstantBool False, ConstantInt 6]]]
                    , ConstantTuple [ConstantBool True , ConstantTuple [ConstantInt 4, ConstantTuple [ConstantBool False, ConstantInt 8]]]
                    ]
            mid = [ ( "x_1", DomainMatrix   (intDomain 1 3) DomainBool
                           , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True] )
                  , ( "x_2", DomainMatrix   (intDomain 1 3) (DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]])
                           , ConstantMatrix (intDomain 1 3)
                               [ ConstantTuple [ConstantInt 2, ConstantTuple [ConstantBool True , ConstantInt 4]]
                               , ConstantTuple [ConstantInt 3, ConstantTuple [ConstantBool False, ConstantInt 6]]
                               , ConstantTuple [ConstantInt 4, ConstantTuple [ConstantBool False, ConstantInt 8]]
                               ] )
                  ]
            low = [ ( "x_1"    , DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ])
                  , ( "x_2_1"  , DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ])
                  , ( "x_2_2_1", DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False])
                  , ( "x_2_2_2", DomainMatrix (intDomain 1 3) (intDomain 2 5), ConstantMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ])
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "matrix of (bool, (int, bool), int)" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool], intDomain 2 5])
            highConstant =
                ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 2, ConstantBool True ], ConstantInt 4]
                    , ConstantTuple [ConstantBool False, ConstantTuple [ConstantInt 3, ConstantBool False], ConstantInt 6]
                    , ConstantTuple [ConstantBool True , ConstantTuple [ConstantInt 4, ConstantBool False], ConstantInt 8]
                    ]
            mid = [ ( "x_1", DomainMatrix   (intDomain 1 3) DomainBool
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
                  ]
            low = [ ( "x_1"  , DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ])
                  , ( "x_2_1", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ])
                  , ( "x_2_2", DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False])
                  , ( "x_3"  , DomainMatrix (intDomain 1 3) (intDomain 2 5), ConstantMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ])
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "matrix of (((bool, int), bool), int)" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool], intDomain 2 5])
            highConstant =
                ConstantMatrix (intDomain 1 3)
                    [ ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True ], ConstantInt 4]
                    , ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantBool False], ConstantInt 6]
                    , ConstantTuple [ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantBool False], ConstantInt 8]
                    ]
            mid = [ ( "x_1", DomainMatrix   (intDomain 1 3) (DomainTuple [DomainTuple [DomainBool,intDomain 1 3],DomainBool])
                           , ConstantMatrix (intDomain 1 3)
                               [ ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 2], ConstantBool True ]
                               , ConstantTuple [ConstantTuple [ConstantBool False, ConstantInt 3], ConstantBool False]
                               , ConstantTuple [ConstantTuple [ConstantBool True , ConstantInt 4], ConstantBool False]
                               ])
                  , ( "x_2", DomainMatrix   (intDomain 1 3) (intDomain 2 5)
                           , ConstantMatrix (intDomain 1 3) [ConstantInt 4, ConstantInt 6, ConstantInt 8]
                           )
                  ]
            low = [ ( "x_1_1_1", DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ])
                  , ( "x_1_1_2", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ])
                  , ( "x_1_2"  , DomainMatrix (intDomain 1 3) DomainBool     , ConstantMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False])
                  , ( "x_2"    , DomainMatrix (intDomain 1 3) (intDomain 2 5), ConstantMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ])
                  ]
        in  testCases "x" highDomain highConstant Just mid low

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
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "(bool, bool, bool)" $ testCasesAuto "x"
        ( DomainTuple [DomainBool, DomainBool, DomainBool] )
        ( ConstantTuple [ConstantBool False, ConstantBool False, ConstantBool True] )

    ]


testCases
    :: Text                                                      -- high level variable name
    -> Domain Representation Constant                            -- high level domain
    -> Constant                                                  -- high level value (constant)
    -> (forall a . a -> Maybe a)                                 -- `const Nothing` -- if going one level down produces Nothing
                                                                 -- `Just`          -- if going one level down produces (Just mid)
    -> [(Text, Domain Representation Constant, Constant)]        -- "mid" result, if we go one level down
    -> [(Text, Domain Representation Constant, Constant)]        -- "low" result, if we go all the way down
    -> [TestTree]
testCases highName highDomain highConstant mkMid mid low =
    [ testCase "down1"   $ down1Test   (highName, highDomain, highConstant) (mkMid mid)
    , testCase "down"    $ downTest    (highName, highDomain, highConstant) low
    , testCase "up1"     $ up1Test     (highName, highDomain) (map dropDomain mid) (highName, highConstant)
    , testCase "up"      $ upTest      (highName, highDomain) (map dropDomain low) (highName, highConstant)
    , testCase "downUp1" $ downUp1Test (highName, highDomain, highConstant)
    , testCase "downUp"  $ downUpTest  (highName, highDomain, highConstant)
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


testCasesAuto
    :: Text                                                      -- high level variable name
    -> Domain Representation Constant                            -- high level domain
    -> Constant                                                  -- high level value (constant)
    -> [TestTree]
testCasesAuto highName highDomain highConstant =
    [ testCase "downUp1" $ downUp1Test (highName, highDomain, highConstant)
    , testCase "downUp"  $ downUpTest  (highName, highDomain, highConstant)
    ]

downUp1Test
    :: (Text, Domain Representation Constant, Constant)
    -> Assertion
downUp1Test high =
    case down1_ dispatch high of
        Left err -> assertFailure (show err)
        Right mlows -> do
            let lows = maybe [dropDomain high] (map dropDomain) mlows   -- use high if we cannot go down1
            case up1 dispatch (dropConstant high) lows of
                Left err -> assertFailure (show err)
                Right high' -> Pr high' @?= Pr (dropDomain high)

downUpTest
    :: (Text, Domain Representation Constant, Constant)
    -> Assertion
downUpTest high =
    case down_ high of
        Left err -> assertFailure (show err)
        Right lows ->
            case up (dropConstant high) (map dropDomain lows) of
                Left err -> assertFailure (show err)
                Right high' -> Pr high' @?= Pr (dropDomain high)


intDomain :: Int -> Int -> Domain r Constant
intDomain lb ub = DomainInt [RangeBounded (ConstantInt lb) (ConstantInt ub)]

dropConstant :: (a,b,c) -> (a,b)
dropConstant (a,b,_) = (a,b)

dropDomain :: (a,b,c) -> (a,c)
dropDomain (a,_,c) = (a,c)


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


