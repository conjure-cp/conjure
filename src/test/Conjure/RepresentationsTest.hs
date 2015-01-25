{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Conjure.RepresentationsTest ( tests ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Representations ( downC, up, downC1, up1 )

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
            highDomain = intDomain 1 4
            highConstant = ConstantInt 3
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testGroup "matrix of bool" $
        let
            highDomain = DomainMatrix (intDomain 1 3) DomainBool
            highConstant = ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True]
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testGroup "matrix of int" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (intDomain 1 5)
            highConstant = ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 2, ConstantInt 3, ConstantInt 5]
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testGroup "matrix 2d of bool" $
        let
            highDomain =
                DomainMatrix (intDomain 1 3) (DomainMatrix (intDomain 1 2) DomainBool)
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantBool False, ConstantBool True ]
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantBool True , ConstantBool False]
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantBool True , ConstantBool True ]
                    ]
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testGroup "matrix 2d of int" $
        let
            highDomain =
                DomainMatrix (intDomain 1 3) (DomainMatrix (intDomain 1 2) (intDomain 0 9))
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 3, ConstantInt 7]
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 2, ConstantInt 8]
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 0, ConstantInt 1]
                    ]
            low = [("x", highDomain, highConstant)]
        in  testCases "x" highDomain highConstant (const Nothing) low low

    , testGroup "(bool, int)" $
        let
            highDomain = DomainTuple [DomainBool, intDomain 1 3]
            highConstant = ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 2]
            low = [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", intDomain 1 3, ConstantInt 2      )
                  ]
        in  testCases "x" highDomain highConstant Just low low

    , testGroup "(bool, int, bool)" $
        let
            highDomain = DomainTuple [DomainBool, intDomain 1 3, DomainBool]
            highConstant = ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 2, ConstantBool True]
            low = [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", intDomain 1 3, ConstantInt 2      )
                  , ( "x_3", DomainBool   , ConstantBool True  )
                  ]
        in  testCases "x" highDomain highConstant Just low low

    , testGroup "((bool, int), bool)" $
        let
            highDomain = DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool]
            highConstant = ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 2], ConstantBool True]
            mid = [ ( "x_1", DomainTuple [DomainBool, intDomain 1 3], ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 2] )
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
            highConstant = ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantAbstract $ AbsLitTuple [ConstantInt 2, ConstantBool True]]
            mid = [ ( "x_1", DomainBool, ConstantBool False )
                  , ( "x_2", DomainTuple [intDomain 1 3, DomainBool], ConstantAbstract $ AbsLitTuple [ConstantInt 2, ConstantBool True] )
                  ]
            low = [ ( "x_1"  , DomainBool   , ConstantBool False )
                  , ( "x_2_1", intDomain 1 3, ConstantInt 2      )
                  , ( "x_2_2", DomainBool   , ConstantBool True  )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "(bool, int, bool, int)" $
        let
            highDomain = DomainTuple [DomainBool, intDomain 1 3, DomainBool, intDomain 2 5]
            highConstant = ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 2, ConstantBool True, ConstantInt 4]
            low = [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", intDomain 1 3, ConstantInt 2      )
                  , ( "x_3", DomainBool   , ConstantBool True  )
                  , ( "x_4", intDomain 2 5, ConstantInt 4      )
                  ]
        in  testCases "x" highDomain highConstant Just low low

    , testGroup "((bool, int), (bool, int))" $
        let
            highDomain = DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainTuple [DomainBool, intDomain 2 5]]
            highConstant = ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 2], ConstantAbstract $ AbsLitTuple [ConstantBool True, ConstantInt 4]]
            mid = [ ( "x_1", DomainTuple [DomainBool, intDomain 1 3], ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 2] )
                  , ( "x_2", DomainTuple [DomainBool, intDomain 2 5], ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantInt 4] )
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
            highConstant = ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantAbstract $ AbsLitTuple [ConstantInt 2, ConstantAbstract $ AbsLitTuple [ConstantBool True, ConstantInt 4]]]
            mid = [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]]
                           , ConstantAbstract $ AbsLitTuple [ConstantInt 2, ConstantAbstract $ AbsLitTuple [ConstantBool True, ConstantInt 4]] )
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
            highConstant = ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantAbstract $ AbsLitTuple [ConstantInt 2, ConstantBool True], ConstantInt 4]
            mid = [ ( "x_1", DomainBool   , ConstantBool False )
                  , ( "x_2", DomainTuple [intDomain 1 3, DomainBool], ConstantAbstract $ AbsLitTuple [ConstantInt 2, ConstantBool True] )
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
            highConstant = ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 2], ConstantBool True], ConstantInt 4]
            mid = [ ( "x_1", DomainTuple [ DomainTuple [DomainBool, intDomain 1 3], DomainBool]
                           , ConstantAbstract $ AbsLitTuple [ ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 2], ConstantBool True] )
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
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 0]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantInt 3]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 4]
                    ]
            low = [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool True, ConstantBool False] )
                  , ( "x_2", DomainMatrix (intDomain 1 3) (intDomain 0 9)
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 0, ConstantInt 3, ConstantInt 4] )
                  ]
        in  testCases "x" highDomain highConstant Just low low

    , testGroup "matrix of (bool, int, bool)" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3, DomainBool])
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 2, ConstantBool True]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 3, ConstantBool False]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 4, ConstantBool False]
                    ]
            low = [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool False] )
                  , ( "x_2", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
                  , ( "x_3", DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
                  ]
        in  testCases "x" highDomain highConstant Just low low

    , testGroup "matrix of ((bool, int), bool)" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool])
            highConstant = 
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 2], ConstantBool True]
                    , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 3], ConstantBool False]
                    , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantInt 4], ConstantBool False]
                    ]
            mid = [ ( "x_1", DomainMatrix   (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3])
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                                [ ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 2]
                                , ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 3]
                                , ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantInt 4]
                                ] )
                  , ( "x_2", DomainMatrix   (intDomain 1 3) DomainBool
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
                  ]
            low = [ ( "x_1_1", DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
                  , ( "x_1_2", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
                  , ( "x_2"  , DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "matrix of (bool, (int, bool))" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 0 9, DomainBool]])
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantAbstract $ AbsLitTuple [ConstantInt 0, ConstantBool True]]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantAbstract $ AbsLitTuple [ConstantInt 3, ConstantBool False]]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantAbstract $ AbsLitTuple [ConstantInt 4, ConstantBool True]]
                    ]
            mid = [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool True, ConstantBool False] )
                  , ( "x_2", DomainMatrix (intDomain 1 3) (DomainTuple [intDomain 0 9, DomainBool])
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                               [ ConstantAbstract $ AbsLitTuple [ConstantInt 0, ConstantBool True]
                               , ConstantAbstract $ AbsLitTuple [ConstantInt 3, ConstantBool False]
                               , ConstantAbstract $ AbsLitTuple [ConstantInt 4, ConstantBool True]
                               ] )
                  ]
            low = [ ( "x_1"  , DomainMatrix (intDomain 1 3) DomainBool
                             , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool True, ConstantBool False] )
                  , ( "x_2_1", DomainMatrix (intDomain 1 3) (intDomain 0 9)
                             , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 0, ConstantInt 3, ConstantInt 4] )
                  , ( "x_2_2", DomainMatrix (intDomain 1 3) DomainBool
                             , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True, ConstantBool False, ConstantBool True] )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "matrix of (bool, int, bool, int)" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3, DomainBool, intDomain 2 5])
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 2, ConstantBool True , ConstantInt 4]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 3, ConstantBool False, ConstantInt 6]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantInt 4, ConstantBool False, ConstantInt 8]
                    ]
            low = [ ( "x_1", DomainMatrix (intDomain 1 3) DomainBool , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
                  , ( "x_2", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
                  , ( "x_3", DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
                  , ( "x_4", DomainMatrix (intDomain 1 3) (intDomain 2 5), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ] )
                  ]
        in  testCases "x" highDomain highConstant Just low low

    , testGroup "matrix of ((bool, int), (bool, int))" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainTuple [DomainBool, intDomain 2 5]])
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 2], ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantInt 4]]
                    , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 3], ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 6]]
                    , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantInt 4], ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 8]]
                    ]
            mid = [ ( "x_1"
                    , DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 1 3])
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                        [ ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 2]
                        , ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 3]
                        , ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantInt 4]
                        ] )
                  , ( "x_2"
                    , DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, intDomain 2 5])
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                        [ ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantInt 4]
                        , ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 6]
                        , ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 8]
                        ] )
                  ]
            low = [ ( "x_1_1", DomainMatrix   (intDomain 1 3) DomainBool
                             , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ] )
                  , ( "x_1_2", DomainMatrix   (intDomain 1 3) (intDomain 1 3)
                             , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ] )
                  , ( "x_2_1", DomainMatrix   (intDomain 1 3) DomainBool
                             , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False] )
                  , ( "x_2_2", DomainMatrix   (intDomain 1 3) (intDomain 2 5)
                             , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ] )
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "matrix of (bool, (int, (bool, int)))" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]]])
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantAbstract $ AbsLitTuple [ConstantInt 2, ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantInt 4]]]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantAbstract $ AbsLitTuple [ConstantInt 3, ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 6]]]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantAbstract $ AbsLitTuple [ConstantInt 4, ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 8]]]
                    ]
            mid = [ ( "x_1", DomainMatrix   (intDomain 1 3) DomainBool
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True] )
                  , ( "x_2", DomainMatrix   (intDomain 1 3) (DomainTuple [intDomain 1 3, DomainTuple [DomainBool, intDomain 2 5]])
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                               [ ConstantAbstract $ AbsLitTuple [ConstantInt 2, ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantInt 4]]
                               , ConstantAbstract $ AbsLitTuple [ConstantInt 3, ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 6]]
                               , ConstantAbstract $ AbsLitTuple [ConstantInt 4, ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 8]]
                               ] )
                  ]
            low = [ ( "x_1"    , DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ])
                  , ( "x_2_1"  , DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ])
                  , ( "x_2_2_1", DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False])
                  , ( "x_2_2_2", DomainMatrix (intDomain 1 3) (intDomain 2 5), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ])
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "matrix of (bool, (int, bool), int)" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainBool, DomainTuple [intDomain 1 3, DomainBool], intDomain 2 5])
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantAbstract $ AbsLitTuple [ConstantInt 2, ConstantBool True ], ConstantInt 4]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantAbstract $ AbsLitTuple [ConstantInt 3, ConstantBool False], ConstantInt 6]
                    , ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantAbstract $ AbsLitTuple [ConstantInt 4, ConstantBool False], ConstantInt 8]
                    ]
            mid = [ ( "x_1", DomainMatrix   (intDomain 1 3) DomainBool
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True] )
                  , ( "x_2", DomainMatrix   (intDomain 1 3) (DomainTuple [intDomain 1 3, DomainBool])
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                               [ ConstantAbstract $ AbsLitTuple [ConstantInt 2, ConstantBool True ]
                               , ConstantAbstract $ AbsLitTuple [ConstantInt 3, ConstantBool False]
                               , ConstantAbstract $ AbsLitTuple [ConstantInt 4, ConstantBool False]
                               ] )
                  , ( "x_3", DomainMatrix   (intDomain 1 3) (intDomain 2 5)
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 4, ConstantInt 6, ConstantInt 8]
                           )
                  ]
            low = [ ( "x_1"  , DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ])
                  , ( "x_2_1", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ])
                  , ( "x_2_2", DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False])
                  , ( "x_3"  , DomainMatrix (intDomain 1 3) (intDomain 2 5), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ])
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "matrix of (((bool, int), bool), int)" $
        let
            highDomain = DomainMatrix (intDomain 1 3) (DomainTuple [DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool], intDomain 2 5])
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                    [ ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 2], ConstantBool True ], ConstantInt 4]
                    , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 3], ConstantBool False], ConstantInt 6]
                    , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantInt 4], ConstantBool False], ConstantInt 8]
                    ]
            mid = [ ( "x_1", DomainMatrix   (intDomain 1 3) (DomainTuple [DomainTuple [DomainBool,intDomain 1 3],DomainBool])
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                               [ ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 2], ConstantBool True ]
                               , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 3], ConstantBool False]
                               , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantInt 4], ConstantBool False]
                               ])
                  , ( "x_2", DomainMatrix   (intDomain 1 3) (intDomain 2 5)
                           , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 4, ConstantInt 6, ConstantInt 8]
                           )
                  ]
            low = [ ( "x_1_1_1", DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False, ConstantBool False, ConstantBool True ])
                  , ( "x_1_1_2", DomainMatrix (intDomain 1 3) (intDomain 1 3), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 2     , ConstantInt 3     , ConstantInt 4     ])
                  , ( "x_1_2"  , DomainMatrix (intDomain 1 3) DomainBool     , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True , ConstantBool False, ConstantBool False])
                  , ( "x_2"    , DomainMatrix (intDomain 1 3) (intDomain 2 5), ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 4     , ConstantInt 6     , ConstantInt 8     ])
                  ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "matrix 2d of (((bool, int), bool), int)" $
        let
            highDomain =
                DomainMatrix (intDomain 1 2)
                    (DomainMatrix (intDomain 1 3)
                        (DomainTuple [DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool], intDomain 2 5]))
            highConstant =
                ConstantAbstract $ AbsLitMatrix (intDomain 1 2)
                    [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                        [ ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 2], ConstantBool True ], ConstantInt 4]
                        , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 3], ConstantBool False], ConstantInt 6]
                        , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantInt 4], ConstantBool False], ConstantInt 8]
                        ]
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                        [ ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantInt 4], ConstantBool True ], ConstantInt 4]
                        , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantInt 5], ConstantBool False], ConstantInt 7]
                        , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True , ConstantInt 6], ConstantBool False], ConstantInt 9]
                        ]
                    ]
            mid =
                [ ( "x_1" , DomainMatrix (intDomain 1 2)
                              (DomainMatrix (intDomain 1 3)
                                  (DomainTuple [DomainTuple [DomainBool, intDomain 1 3], DomainBool]))
                          , ConstantAbstract $ AbsLitMatrix (intDomain 1 2)
                              [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                                  [ ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False,ConstantInt 2],ConstantBool True]
                                  , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False,ConstantInt 3],ConstantBool False]
                                  , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True,ConstantInt 4],ConstantBool False]
                                  ]
                              , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                                  [ ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool False,ConstantInt 4],ConstantBool True]
                                  , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True,ConstantInt 5],ConstantBool False]
                                  , ConstantAbstract $ AbsLitTuple [ConstantAbstract $ AbsLitTuple [ConstantBool True,ConstantInt 6],ConstantBool False]
                                  ]
                              ] )
                , ( "x_2" , DomainMatrix (intDomain 1 2)
                                  (DomainMatrix (intDomain 1 3)
                                      (intDomain 2 5))
                          , ConstantAbstract $ AbsLitMatrix (intDomain 1 2)
                                [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 4,ConstantInt 6,ConstantInt 8]
                                , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 4,ConstantInt 7,ConstantInt 9]
                                ] )
                ]
            low =
                [ ( "x_1_1_1" , DomainMatrix   (intDomain 1 2) (DomainMatrix (intDomain 1 3) DomainBool)
                              , ConstantAbstract $ AbsLitMatrix (intDomain 1 2)
                                  [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False,ConstantBool False,ConstantBool True]
                                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False,ConstantBool True,ConstantBool True]
                                  ] )
                , ( "x_1_1_2" , DomainMatrix   (intDomain 1 2) (DomainMatrix (intDomain 1 3) (intDomain 1 3))
                              , ConstantAbstract $ AbsLitMatrix (intDomain 1 2)
                                  [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 2,ConstantInt 3,ConstantInt 4]
                                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 4,ConstantInt 5,ConstantInt 6]
                                  ] )
                , ( "x_1_2"   , DomainMatrix   (intDomain 1 2) (DomainMatrix (intDomain 1 3) DomainBool)
                              , ConstantAbstract $ AbsLitMatrix (intDomain 1 2)
                                  [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True,ConstantBool False,ConstantBool False]
                                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True,ConstantBool False,ConstantBool False]
                                  ] )
                , ( "x_2"     , DomainMatrix   (intDomain 1 2) (DomainMatrix (intDomain 1 3) (intDomain 2 5))
                              , ConstantAbstract $ AbsLitMatrix (intDomain 1 2)
                                  [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 4,ConstantInt 6,ConstantInt 8]
                                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 4,ConstantInt 7,ConstantInt 9]
                                  ] )
                ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "(bool, bool, bool)" $ testCasesAuto "x"
        ( DomainTuple [DomainBool, DomainBool, DomainBool] )
        ( ConstantAbstract $ AbsLitTuple [ConstantBool False, ConstantBool False, ConstantBool True] )

    , testGroup "(bool, matrix of int) {auto}" $ testCasesAuto "x"
        ( DomainTuple
            [ DomainBool
            , DomainMatrix (intDomain 1 3) (intDomain 0 9)
            ] )
        ( ConstantAbstract $ AbsLitTuple
            [ ConstantBool False
            , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 2, ConstantInt 4, ConstantInt 5]
            ] )

    , testGroup "(bool, matrix of int)" $
        let
            highDomain =
                DomainTuple
                    [ DomainBool
                    , DomainMatrix (intDomain 1 3) (intDomain 0 9)
                    ]
            highConstant =
                ConstantAbstract $ AbsLitTuple
                    [ ConstantBool False
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 2, ConstantInt 4, ConstantInt 5]
                    ]
            low =
                [ ( "x_1", DomainBool,ConstantBool False)
                , ( "x_2", DomainMatrix (intDomain 1 3) (intDomain 0 9)
                         , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 2,ConstantInt 4,ConstantInt 5] )
                ]
        in  testCases "x" highDomain highConstant Just low low

    , testGroup "(bool, matrix of (int, bool)) {auto}" $ testCasesAuto "x"
        ( DomainTuple
            [ DomainBool
            , DomainMatrix (intDomain 1 3) (DomainTuple [intDomain 0 9, DomainBool])
            ] )
        ( ConstantAbstract $ AbsLitTuple
            [ ConstantBool False
            , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                [ ConstantAbstract $ AbsLitTuple [ConstantInt 2, ConstantBool False]
                , ConstantAbstract $ AbsLitTuple [ConstantInt 4, ConstantBool True]
                , ConstantAbstract $ AbsLitTuple [ConstantInt 5, ConstantBool False]
                ]
            ] )

    , testGroup "(bool, matrix of (int, bool))" $
        let
            highDomain =
                DomainTuple
                    [ DomainBool
                    , DomainMatrix (intDomain 1 3) (DomainTuple [intDomain 0 9, DomainBool])
                    ]
            highConstant =
                ConstantAbstract $ AbsLitTuple
                    [ ConstantBool False
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                        [ ConstantAbstract $ AbsLitTuple [ConstantInt 2, ConstantBool False]
                        , ConstantAbstract $ AbsLitTuple [ConstantInt 4, ConstantBool True]
                        , ConstantAbstract $ AbsLitTuple [ConstantInt 5, ConstantBool False]
                        ]
                    ]
            mid =
                [ ( "x_1" , DomainBool , ConstantBool False )
                , ( "x_2" , DomainMatrix (intDomain 1 3) (DomainTuple [intDomain 0 9,DomainBool])
                          , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                              [ ConstantAbstract $ AbsLitTuple [ConstantInt 2,ConstantBool False]
                              , ConstantAbstract $ AbsLitTuple [ConstantInt 4,ConstantBool True]
                              , ConstantAbstract $ AbsLitTuple [ConstantInt 5,ConstantBool False]
                              ] )
                ]
            low =
                [ ( "x_1"   , DomainBool , ConstantBool False )
                , ( "x_2_1" , DomainMatrix (intDomain 1 3) (intDomain 0 9) , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 2,ConstantInt 4,ConstantInt 5] )
                , ( "x_2_2" , DomainMatrix (intDomain 1 3) DomainBool      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False,ConstantBool True,ConstantBool False] )
                ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "(bool, matrix of (int, matrix of int)) {auto}" $ testCasesAuto "x"
        ( DomainTuple
            [ DomainBool
            , DomainMatrix (intDomain 1 3) (DomainTuple
                [ intDomain 0 9
                , DomainMatrix (intDomain 1 2) (intDomain 0 9)
                ])
            ] )
        ( ConstantAbstract $ AbsLitTuple
            [ ConstantBool False
            , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                [ ConstantAbstract $ AbsLitTuple [ConstantInt 2, ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 1, ConstantInt 3]]
                , ConstantAbstract $ AbsLitTuple [ConstantInt 4, ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 3, ConstantInt 5]]
                , ConstantAbstract $ AbsLitTuple [ConstantInt 5, ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 5, ConstantInt 6]]
                ]
            ] )

    , testGroup "(bool, matrix of (int, matrix of int))" $
        let
            highDomain = 
                DomainTuple
                    [ DomainBool
                    , DomainMatrix (intDomain 1 3) (DomainTuple
                        [ intDomain 0 9
                        , DomainMatrix (intDomain 1 2) (intDomain 0 9)
                        ])
                    ]
            highConstant =
                ConstantAbstract $ AbsLitTuple
                    [ ConstantBool False
                    , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                        [ ConstantAbstract $ AbsLitTuple [ConstantInt 2, ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 1, ConstantInt 3]]
                        , ConstantAbstract $ AbsLitTuple [ConstantInt 4, ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 3, ConstantInt 5]]
                        , ConstantAbstract $ AbsLitTuple [ConstantInt 5, ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 5, ConstantInt 6]]
                        ]
                    ]
            mid =
                [ ( "x_1" , DomainBool,ConstantBool False )
                , ( "x_2" , DomainMatrix   (intDomain 1 3) (DomainTuple [intDomain 0 9,DomainMatrix (intDomain 1 2) (intDomain 0 9)])
                          , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                              [ ConstantAbstract $ AbsLitTuple [ConstantInt 2,ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 1,ConstantInt 3]]
                              , ConstantAbstract $ AbsLitTuple [ConstantInt 4,ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 3,ConstantInt 5]]
                              , ConstantAbstract $ AbsLitTuple [ConstantInt 5,ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 5,ConstantInt 6]]
                              ] )
                ]
            low =
                [ ( "x_1"   , DomainBool,ConstantBool False )
                , ( "x_2_1" , DomainMatrix   (intDomain 1 3) (intDomain 0 9)
                            , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 2,ConstantInt 4,ConstantInt 5])
                , ( "x_2_2" , DomainMatrix   (intDomain 1 3) (DomainMatrix (intDomain 1 2) (intDomain 0 9))
                            , ConstantAbstract $ AbsLitMatrix (intDomain 1 3)
                                [ ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 1,ConstantInt 3]
                                , ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 3,ConstantInt 5]
                                , ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 5,ConstantInt 6]
                                ] )
                ]
        in  testCases "x" highDomain highConstant Just mid low

-- Explicit
    , testGroup "Explicit: set (size 4) of int {auto}" $ testCasesAuto "x"
        ( DomainSet
            "Explicit"
            (SetAttr (SizeAttr_Size (ConstantInt 4)))
            (intDomain 0 9) )
        ( ConstantAbstract $ AbsLitSet
            [ConstantInt 2, ConstantInt 3, ConstantInt 5, ConstantInt 6] )

    , testGroup "Explicit: set (size 4) of int" $
        let
            highDomain =
                DomainSet
                    "Explicit"
                    (SetAttr (SizeAttr_Size (ConstantInt 4)))
                    (intDomain 0 9)
            highConstant =
                ConstantAbstract $ AbsLitSet
                    [ConstantInt 2, ConstantInt 3, ConstantInt 5, ConstantInt 6]
            low =
                [ ( "x_Explicit"
                  , DomainMatrix   (intDomain 1 4) (intDomain 0 9)
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                        [ConstantInt 2,ConstantInt 3,ConstantInt 5,ConstantInt 6]
                  ) ]
        in  testCases "x" highDomain highConstant Just low low

    , testGroup "Explicit: set (size 4) of set (size 2) of int {auto}" $ testCasesAuto "x"
        ( DomainSet "Explicit" (SetAttr (SizeAttr_Size (ConstantInt 4)))
            ( DomainSet "Explicit" (SetAttr (SizeAttr_Size (ConstantInt 2)))
                (intDomain 0 9)
            )
        )
        ( ConstantAbstract $ AbsLitSet
            [ ConstantAbstract $ AbsLitSet [ConstantInt 2, ConstantInt 3]
            , ConstantAbstract $ AbsLitSet [ConstantInt 5, ConstantInt 6]
            , ConstantAbstract $ AbsLitSet [ConstantInt 5, ConstantInt 7]
            , ConstantAbstract $ AbsLitSet [ConstantInt 5, ConstantInt 8]
            ] )

    , testGroup "Explicit: set (size 4) of set (size 2) of int" $
        let
            highDomain =
                DomainSet "Explicit" (SetAttr (SizeAttr_Size (ConstantInt 4)))
                    (DomainSet "Explicit" (SetAttr (SizeAttr_Size (ConstantInt 2)))
                        (intDomain 0 9))
            highConstant =
                ConstantAbstract $ AbsLitSet
                    [ ConstantAbstract $ AbsLitSet [ConstantInt 2, ConstantInt 3]
                    , ConstantAbstract $ AbsLitSet [ConstantInt 5, ConstantInt 6]
                    , ConstantAbstract $ AbsLitSet [ConstantInt 5, ConstantInt 7]
                    , ConstantAbstract $ AbsLitSet [ConstantInt 5, ConstantInt 8]
                    ]
            mid =
                [ ( "x_Explicit"
                  , DomainMatrix   (intDomain 1 4) (DomainSet "Explicit" (SetAttr (SizeAttr_Size (ConstantInt 2))) (intDomain 0 9))
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                        [ ConstantAbstract $ AbsLitSet [ConstantInt 2, ConstantInt 3]
                        , ConstantAbstract $ AbsLitSet [ConstantInt 5, ConstantInt 6]
                        , ConstantAbstract $ AbsLitSet [ConstantInt 5, ConstantInt 7]
                        , ConstantAbstract $ AbsLitSet [ConstantInt 5, ConstantInt 8]
                        ]
                  ) ]
            low =
                [ ( "x_Explicit_Explicit"
                  , DomainMatrix   (intDomain 1 4) (DomainMatrix (intDomain 1 2) (intDomain 0 9))
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                        [ ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 2, ConstantInt 3]
                        , ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 5, ConstantInt 6]
                        , ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 5, ConstantInt 7]
                        , ConstantAbstract $ AbsLitMatrix (intDomain 1 2) [ConstantInt 5, ConstantInt 8]
                        ]
                  ) ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "Explicit: set (size 4) of set (size 2) of (int, bool) {auto}" $ testCasesAuto "x"
        ( DomainSet "Explicit" (SetAttr (SizeAttr_Size (ConstantInt 4)))
            ( DomainSet "Explicit" (SetAttr (SizeAttr_Size (ConstantInt 2)))
                (DomainTuple [intDomain 0 9, DomainBool])
            )
        )
        ( ConstantAbstract $ AbsLitSet
            [ ConstantAbstract $ AbsLitSet [ ConstantAbstract $ AbsLitTuple [ConstantInt 2, ConstantBool False]
                          , ConstantAbstract $ AbsLitTuple [ConstantInt 3, ConstantBool True ]
                          ]
            , ConstantAbstract $ AbsLitSet [ ConstantAbstract $ AbsLitTuple [ConstantInt 5, ConstantBool True ]
                          , ConstantAbstract $ AbsLitTuple [ConstantInt 6, ConstantBool True ]
                          ]
            , ConstantAbstract $ AbsLitSet [ ConstantAbstract $ AbsLitTuple [ConstantInt 5, ConstantBool True ]
                          , ConstantAbstract $ AbsLitTuple [ConstantInt 7, ConstantBool False]
                          ]
            , ConstantAbstract $ AbsLitSet [ ConstantAbstract $ AbsLitTuple [ConstantInt 5, ConstantBool False]
                          , ConstantAbstract $ AbsLitTuple [ConstantInt 8, ConstantBool False]
                          ]
            ] )

    , testGroup "Explicit: set (size 4) of (int, set (size 2) of (int, bool)) {auto}" $ testCasesAuto "x"
        ( DomainSet "Explicit" (SetAttr (SizeAttr_Size (ConstantInt 4)))
            ( DomainTuple
                [ intDomain 0 8
                , DomainSet "Explicit" (SetAttr (SizeAttr_Size (ConstantInt 2)))
                    (DomainTuple [intDomain 0 9, DomainBool])
                ]
            )
        )
        ( ConstantAbstract $ AbsLitSet
            [ ConstantAbstract $ AbsLitTuple
                [ ConstantInt 1
                , ConstantAbstract $ AbsLitSet
                    [ ConstantAbstract $ AbsLitTuple [ConstantInt 2, ConstantBool False]
                    , ConstantAbstract $ AbsLitTuple [ConstantInt 3, ConstantBool True ]
                    ]
                ]
            , ConstantAbstract $ AbsLitTuple
                [ ConstantInt 2
                , ConstantAbstract $ AbsLitSet
                    [ ConstantAbstract $ AbsLitTuple [ConstantInt 5, ConstantBool True ]
                    , ConstantAbstract $ AbsLitTuple [ConstantInt 6, ConstantBool True ]
                    ]
                ]
            , ConstantAbstract $ AbsLitTuple
                [ ConstantInt 3
                , ConstantAbstract $ AbsLitSet
                    [ ConstantAbstract $ AbsLitTuple [ConstantInt 5, ConstantBool True ]
                    , ConstantAbstract $ AbsLitTuple [ConstantInt 7, ConstantBool False]
                    ]
                ]
            , ConstantAbstract $ AbsLitTuple
                [ ConstantInt 4
                , ConstantAbstract $ AbsLitSet
                    [ ConstantAbstract $ AbsLitTuple [ConstantInt 5, ConstantBool False]
                    , ConstantAbstract $ AbsLitTuple [ConstantInt 8, ConstantBool False]
                    ]
                ]
            ] )

-- ExplicitVarSizeWithMarker
    , testGroup "ExplicitVarSizeWithMarker: set (maxSize 4) of int {auto}" $ testCasesAuto "x"
        ( DomainSet
            "ExplicitVarSizeWithMarker"
            (SetAttr (SizeAttr_MaxSize (ConstantInt 4)))
            (intDomain 0 9) )
        ( ConstantAbstract $ AbsLitSet [ConstantInt 2, ConstantInt 5] )

    , testGroup "ExplicitVarSizeWithMarker: set (maxSize 4) of int" $
        let
            highDomain =
                DomainSet "ExplicitVarSizeWithMarker" (SetAttr (SizeAttr_MaxSize (ConstantInt 4))) (intDomain 0 9)
            highConstant =
                ConstantAbstract $ AbsLitSet [ConstantInt 2, ConstantInt 5]
            low =
                [ ( "x_ExplicitVarSizeWithMarker_Marker"
                  , intDomain 0 4
                  , ConstantInt 2
                  )
                , ( "x_ExplicitVarSizeWithMarker_Values"
                  , DomainMatrix (intDomain 1 4) (intDomain 0 9)
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4) [ConstantInt 2,ConstantInt 5,ConstantInt 0,ConstantInt 0]
                  )
                ]
        in  testCases "x" highDomain highConstant Just low low

    , testGroup "ExplicitVarSizeWithMarker: set (maxSize 4) of set (maxSize 3) int {auto}" $ testCasesAuto "x"
        ( DomainSet "ExplicitVarSizeWithMarker" (SetAttr (SizeAttr_MaxSize (ConstantInt 4)))
            ( DomainSet "ExplicitVarSizeWithMarker" (SetAttr (SizeAttr_MaxSize (ConstantInt 3)))
                (intDomain 0 9)
            )
        )
        ( ConstantAbstract $ AbsLitSet
            [ ConstantAbstract $ AbsLitSet [ConstantInt 2]
            , ConstantAbstract $ AbsLitSet [ConstantInt 2, ConstantInt 5]
            , ConstantAbstract $ AbsLitSet [ConstantInt 3, ConstantInt 4, ConstantInt 6]
            ]
        )

    , testGroup "ExplicitVarSizeWithMarker: set (maxSize 4) of set (maxSize 3) int" $
        let
            highDomain =
                DomainSet "ExplicitVarSizeWithMarker" (SetAttr (SizeAttr_MaxSize (ConstantInt 4)))
                    ( DomainSet "ExplicitVarSizeWithMarker" (SetAttr (SizeAttr_MaxSize (ConstantInt 3)))
                        (intDomain 0 9) )
            highConstant =
                ConstantAbstract $ AbsLitSet
                    [ ConstantAbstract $ AbsLitSet [ConstantInt 2]
                    , ConstantAbstract $ AbsLitSet [ConstantInt 2, ConstantInt 5]
                    , ConstantAbstract $ AbsLitSet [ConstantInt 3, ConstantInt 4, ConstantInt 6]
                    ]
            mid =
                [ ( "x_ExplicitVarSizeWithMarker_Marker"
                  , intDomain 0 4
                  , ConstantInt 3
                  )
                , ( "x_ExplicitVarSizeWithMarker_Values"
                  , DomainMatrix   (intDomain 1 4) (DomainSet "ExplicitVarSizeWithMarker" (SetAttr (SizeAttr_MaxSize (ConstantInt 3))) (intDomain 0 9))
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                      [ ConstantAbstract $ AbsLitSet [ConstantInt 2]
                      , ConstantAbstract $ AbsLitSet [ConstantInt 2,ConstantInt 5]
                      , ConstantAbstract $ AbsLitSet [ConstantInt 3,ConstantInt 4,ConstantInt 6]
                      , ConstantAbstract $ AbsLitSet []
                      ]
                  )
                ]
            low =
                [ ( "x_ExplicitVarSizeWithMarker_Marker"
                  , intDomain 0 4
                  , ConstantInt 3
                  )
                , ( "x_ExplicitVarSizeWithMarker_Values_ExplicitVarSizeWithMarker_Marker"
                  , DomainMatrix   (intDomain 1 4) (intDomain 0 3)
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4) [ConstantInt 1,ConstantInt 2,ConstantInt 3,ConstantInt 0]
                  )
                , ( "x_ExplicitVarSizeWithMarker_Values_ExplicitVarSizeWithMarker_Values"
                  , DomainMatrix   (intDomain 1 4) (DomainMatrix (intDomain 1 3) (intDomain 0 9))
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                      [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 2,ConstantInt 0,ConstantInt 0]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 2,ConstantInt 5,ConstantInt 0]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 3,ConstantInt 4,ConstantInt 6]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 0,ConstantInt 0,ConstantInt 0]
                      ]
                  )
                ]
        in  testCases "x" highDomain highConstant Just mid low

-- ExplicitVarSizeWithFlags
    , testGroup "ExplicitVarSizeWithFlags: set (maxSize 4) of int {auto}" $ testCasesAuto "x"
        ( DomainSet
            "ExplicitVarSizeWithFlags"
            (SetAttr (SizeAttr_MaxSize (ConstantInt 4)))
            (intDomain 0 9) )
        ( ConstantAbstract $ AbsLitSet [ConstantInt 2, ConstantInt 5] )

    , testGroup "ExplicitVarSizeWithFlags: set (maxSize 4) of int" $
        let
            highDomain =
                DomainSet "ExplicitVarSizeWithFlags" (SetAttr (SizeAttr_MaxSize (ConstantInt 4))) (intDomain 0 9)
            highConstant =
                ConstantAbstract $ AbsLitSet [ConstantInt 2, ConstantInt 5]
            low =
                [ ( "x_ExplicitVarSizeWithFlags_Flags"
                  , DomainMatrix   (intDomain 1 4) DomainBool
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4) [ConstantBool True,ConstantBool True,ConstantBool False,ConstantBool False]
                  )
                , ( "x_ExplicitVarSizeWithFlags_Values"
                  , DomainMatrix   (intDomain 1 4) (intDomain 0 9)
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4) [ConstantInt 2,ConstantInt 5,ConstantInt 0,ConstantInt 0]
                  )
                ]
        in  testCases "x" highDomain highConstant Just low low

    , testGroup "ExplicitVarSizeWithFlags: set (maxSize 4) of set (maxSize 3) int {auto}" $ testCasesAuto "x"
        ( DomainSet "ExplicitVarSizeWithFlags" (SetAttr (SizeAttr_MaxSize (ConstantInt 4)))
            ( DomainSet "ExplicitVarSizeWithFlags" (SetAttr (SizeAttr_MaxSize (ConstantInt 3)))
                (intDomain 0 9)
            )
        )
        ( ConstantAbstract $ AbsLitSet
            [ ConstantAbstract $ AbsLitSet [ConstantInt 2]
            , ConstantAbstract $ AbsLitSet [ConstantInt 2, ConstantInt 5]
            , ConstantAbstract $ AbsLitSet [ConstantInt 3, ConstantInt 4, ConstantInt 6]
            ]
        )

    , testGroup "ExplicitVarSizeWithFlags: set (maxSize 4) of set (maxSize 3) int" $
        let
            highDomain =
                DomainSet "ExplicitVarSizeWithFlags" (SetAttr (SizeAttr_MaxSize (ConstantInt 4)))
                    ( DomainSet "ExplicitVarSizeWithFlags" (SetAttr (SizeAttr_MaxSize (ConstantInt 3)))
                        (intDomain 0 9) )
            highConstant =
                ConstantAbstract $ AbsLitSet
                    [ ConstantAbstract $ AbsLitSet [ConstantInt 2]
                    , ConstantAbstract $ AbsLitSet [ConstantInt 2, ConstantInt 5]
                    , ConstantAbstract $ AbsLitSet [ConstantInt 3, ConstantInt 4, ConstantInt 6]
                    ]
            mid =
                [ ( "x_ExplicitVarSizeWithFlags_Flags"
                  , DomainMatrix   (intDomain 1 4) DomainBool
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4) [ConstantBool True,ConstantBool True,ConstantBool True,ConstantBool False]
                  )
                , ( "x_ExplicitVarSizeWithFlags_Values"
                  , DomainMatrix   (intDomain 1 4) (DomainSet "ExplicitVarSizeWithFlags" (SetAttr (SizeAttr_MaxSize (ConstantInt 3))) (intDomain 0 9))
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                      [ ConstantAbstract $ AbsLitSet [ConstantInt 2]
                      , ConstantAbstract $ AbsLitSet [ConstantInt 2,ConstantInt 5]
                      , ConstantAbstract $ AbsLitSet [ConstantInt 3,ConstantInt 4,ConstantInt 6]
                      , ConstantAbstract $ AbsLitSet []
                      ]
                  )
                ]
            low =
                [ ( "x_ExplicitVarSizeWithFlags_Flags"
                  , DomainMatrix   (intDomain 1 4) DomainBool
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4) [ConstantBool True,ConstantBool True,ConstantBool True,ConstantBool False]
                  )
                , ( "x_ExplicitVarSizeWithFlags_Values_ExplicitVarSizeWithFlags_Flags"
                  , DomainMatrix   (intDomain 1 4) (DomainMatrix (intDomain 1 3) DomainBool)
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                      [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True,ConstantBool False,ConstantBool False]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True,ConstantBool True,ConstantBool False]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool True,ConstantBool True,ConstantBool True]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantBool False,ConstantBool False,ConstantBool False]
                      ]
                  )
                , ( "x_ExplicitVarSizeWithFlags_Values_ExplicitVarSizeWithFlags_Values"
                  , DomainMatrix   (intDomain 1 4) (DomainMatrix (intDomain 1 3) (intDomain 0 9))
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                      [ ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 2,ConstantInt 0,ConstantInt 0]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 2,ConstantInt 5,ConstantInt 0]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 3,ConstantInt 4,ConstantInt 6]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 1 3) [ConstantInt 0,ConstantInt 0,ConstantInt 0]
                      ]
                  ) ]
        in  testCases "x" highDomain highConstant Just mid low

-- Occurrence
    , testGroup "Occurrence: set (maxSize 4) of int {auto}" $ testCasesAuto "x"
        ( DomainSet
            "Occurrence"
            (SetAttr (SizeAttr_MaxSize (ConstantInt 4)))
            (intDomain 0 9) )
        ( ConstantAbstract $ AbsLitSet [ConstantInt 2, ConstantInt 5] )

    , testGroup "Occurrence: set (maxSize 4) of int" $
        let
            highDomain =
                DomainSet "Occurrence" (SetAttr (SizeAttr_MaxSize (ConstantInt 4))) (intDomain 0 9)
            highConstant =
                ConstantAbstract $ AbsLitSet [ConstantInt 2, ConstantInt 5]
            low =
                [ ( "x_Occurrence"
                  , DomainMatrix   (intDomain 0 9) DomainBool
                  , ConstantAbstract $ AbsLitMatrix (intDomain 0 9)
                      [ ConstantBool False
                      , ConstantBool False
                      , ConstantBool True -- 2
                      , ConstantBool False
                      , ConstantBool False
                      , ConstantBool True -- 5
                      , ConstantBool False
                      , ConstantBool False
                      , ConstantBool False
                      , ConstantBool False
                      ]
                  )
                ]
        in  testCases "x" highDomain highConstant Just low low

    , testGroup "ExplicitVarSizeWithMarker & Occurrence: set (maxSize 4) of set (maxSize 3) int {auto}" $ testCasesAuto "x"
        ( DomainSet "ExplicitVarSizeWithMarker" (SetAttr (SizeAttr_MaxSize (ConstantInt 4)))
            ( DomainSet "Occurrence" (SetAttr (SizeAttr_MaxSize (ConstantInt 3)))
                (intDomain 0 9)
            )
        )
        ( ConstantAbstract $ AbsLitSet
            [ ConstantAbstract $ AbsLitSet [ConstantInt 2]
            , ConstantAbstract $ AbsLitSet [ConstantInt 2, ConstantInt 5]
            , ConstantAbstract $ AbsLitSet [ConstantInt 3, ConstantInt 4, ConstantInt 6]
            ]
        )

    , testGroup "ExplicitVarSizeWithMarker & Occurrence: set (maxSize 4) of set (maxSize 3) int" $
        let
            highDomain =
                DomainSet "ExplicitVarSizeWithMarker" (SetAttr (SizeAttr_MaxSize (ConstantInt 4)))
                    ( DomainSet "Occurrence" (SetAttr (SizeAttr_MaxSize (ConstantInt 3)))
                        (intDomain 0 9) )
            highConstant =
                ConstantAbstract $ AbsLitSet
                    [ ConstantAbstract $ AbsLitSet [ConstantInt 2]
                    , ConstantAbstract $ AbsLitSet [ConstantInt 2, ConstantInt 5]
                    , ConstantAbstract $ AbsLitSet [ConstantInt 3, ConstantInt 4, ConstantInt 6]
                    ]
            mid =
                [ ( "x_ExplicitVarSizeWithMarker_Marker"
                  , intDomain 0 4
                  , ConstantInt 3
                  )
                , ( "x_ExplicitVarSizeWithMarker_Values"
                  , DomainMatrix   (intDomain 1 4) (DomainSet "Occurrence" (SetAttr (SizeAttr_MaxSize (ConstantInt 3))) (intDomain 0 9))
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                      [ ConstantAbstract $ AbsLitSet [ConstantInt 2]
                      , ConstantAbstract $ AbsLitSet [ConstantInt 2,ConstantInt 5]
                      , ConstantAbstract $ AbsLitSet [ConstantInt 3,ConstantInt 4,ConstantInt 6]
                      , ConstantAbstract $ AbsLitSet []
                      ]
                  )
                ]
            low =
                [ ( "x_ExplicitVarSizeWithMarker_Marker"
                  , intDomain 0 4
                  , ConstantInt 3
                  )
                , ( "x_ExplicitVarSizeWithMarker_Values_Occurrence"
                  , DomainMatrix   (intDomain 1 4) (DomainMatrix (intDomain 0 9) DomainBool)
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                      [ ConstantAbstract $ AbsLitMatrix (intDomain 0 9) -- 2
                          [ ConstantBool False, ConstantBool False, ConstantBool True , ConstantBool False, ConstantBool False
                          , ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False ]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 0 9) -- 2,5
                          [ ConstantBool False, ConstantBool False, ConstantBool True , ConstantBool False, ConstantBool False
                          , ConstantBool True , ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False ]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 0 9) -- 3,4,6
                          [ ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool True , ConstantBool True
                          , ConstantBool False, ConstantBool True , ConstantBool False, ConstantBool False, ConstantBool False ]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 0 9) -- {}
                          [ ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False
                          , ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False ]
                      ]
                  )
                ]
        in  testCases "x" highDomain highConstant Just mid low

    , testGroup "ExplicitVarSizeWithFlags & Occurrence: set (maxSize 4) of set (maxSize 3) int {auto}" $ testCasesAuto "x"
        ( DomainSet "ExplicitVarSizeWithFlags" (SetAttr (SizeAttr_MaxSize (ConstantInt 4)))
            ( DomainSet "Occurrence" (SetAttr (SizeAttr_MaxSize (ConstantInt 3)))
                (intDomain 0 9)
            )
        )
        ( ConstantAbstract $ AbsLitSet
            [ ConstantAbstract $ AbsLitSet [ConstantInt 2]
            , ConstantAbstract $ AbsLitSet [ConstantInt 2, ConstantInt 5]
            , ConstantAbstract $ AbsLitSet [ConstantInt 3, ConstantInt 4, ConstantInt 6]
            ]
        )

    , testGroup "ExplicitVarSizeWithFlags & Occurrence: set (maxSize 4) of set (maxSize 3) int" $
        let
            highDomain =
                DomainSet "ExplicitVarSizeWithFlags" (SetAttr (SizeAttr_MaxSize (ConstantInt 4)))
                    ( DomainSet "Occurrence" (SetAttr (SizeAttr_MaxSize (ConstantInt 3)))
                        (intDomain 0 9) )
            highConstant =
                ConstantAbstract $ AbsLitSet
                    [ ConstantAbstract $ AbsLitSet [ConstantInt 2]
                    , ConstantAbstract $ AbsLitSet [ConstantInt 2, ConstantInt 5]
                    , ConstantAbstract $ AbsLitSet [ConstantInt 3, ConstantInt 4, ConstantInt 6]
                    ]
            mid =
                [ ( "x_ExplicitVarSizeWithFlags_Flags"
                  , DomainMatrix   (intDomain 1 4) DomainBool
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4) [ConstantBool True,ConstantBool True,ConstantBool True,ConstantBool False]
                  )
                , ( "x_ExplicitVarSizeWithFlags_Values"
                  , DomainMatrix   (intDomain 1 4) (DomainSet "Occurrence" (SetAttr (SizeAttr_MaxSize (ConstantInt 3))) (intDomain 0 9))
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                      [ ConstantAbstract $ AbsLitSet [ConstantInt 2]
                      , ConstantAbstract $ AbsLitSet [ConstantInt 2,ConstantInt 5]
                      , ConstantAbstract $ AbsLitSet [ConstantInt 3,ConstantInt 4,ConstantInt 6]
                      , ConstantAbstract $ AbsLitSet []
                      ]
                  )
                ]
            low =
                [ ( "x_ExplicitVarSizeWithFlags_Flags"
                  , DomainMatrix   (intDomain 1 4) DomainBool
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4) [ConstantBool True,ConstantBool True,ConstantBool True,ConstantBool False]
                  )
                , ( "x_ExplicitVarSizeWithFlags_Values_Occurrence"
                  , DomainMatrix   (intDomain 1 4) (DomainMatrix (intDomain 0 9) DomainBool)
                  , ConstantAbstract $ AbsLitMatrix (intDomain 1 4)
                      [ ConstantAbstract $ AbsLitMatrix (intDomain 0 9) -- 2
                          [ ConstantBool False, ConstantBool False, ConstantBool True , ConstantBool False, ConstantBool False
                          , ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False ]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 0 9) -- 2,5
                          [ ConstantBool False, ConstantBool False, ConstantBool True , ConstantBool False, ConstantBool False
                          , ConstantBool True , ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False ]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 0 9) -- 3,4,6
                          [ ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool True , ConstantBool True
                          , ConstantBool False, ConstantBool True , ConstantBool False, ConstantBool False, ConstantBool False ]
                      , ConstantAbstract $ AbsLitMatrix (intDomain 0 9) -- {}
                          [ ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False
                          , ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False, ConstantBool False ]
                      ]
                  )
                ]
        in  testCases "x" highDomain highConstant Just mid low

    ]


testCases
    :: Name                                                      -- high level variable name
    -> Domain HasRepresentation Constant                         -- high level domain
    -> Constant                                                  -- high level value (constant)
    -> (forall a . a -> Maybe a)                                 -- `const Nothing` -- if going one level downC produces Nothing
                                                                 -- `Just`          -- if going one level downC produces (Just mid)
    -> [(Name, Domain HasRepresentation Constant, Constant)]     -- "mid" result, if we go one level down
    -> [(Name, Domain HasRepresentation Constant, Constant)]     -- "low" result, if we go all the way down
    -> [TestTree]
testCases highName highDomain highConstant mkMid mid low =
    [ testCase "downC1"  $ downC1Test  (highName, highDomain, highConstant) (mkMid mid)
    , testCase "down"    $ downTest    (highName, highDomain, highConstant) low
    , testCase "up1"     $ up1Test     (highName, highDomain) (map dropDomain mid) (highName, highConstant)
    , testCase "up"      $ upTest      (highName, highDomain) (map dropDomain low) (highName, highConstant)
    , testCase "downUp1" $ downUp1Test (highName, highDomain, highConstant)
    , testCase "downUp"  $ downUpTest  (highName, highDomain, highConstant)
    ]

downC1Test
    :: (Name, Domain HasRepresentation Constant, Constant)
    -> Maybe [(Name, Domain HasRepresentation Constant, Constant)]
    -> Assertion
downC1Test high low' =
    case downC1 high of
        Left err -> assertFailure (show err)
        Right low -> Pr low @?= Pr low'

downTest
    :: (Name, Domain HasRepresentation Constant, Constant)
    -> [(Name, Domain HasRepresentation Constant, Constant)]
    -> Assertion
downTest high lows' =
    case downC high of
        Left err -> assertFailure (show err)
        Right lows -> Pr lows @?= Pr lows'

up1Test
    :: (Name, Domain HasRepresentation Constant)
    -> [(Name, Constant)]
    -> (Name, Constant)
    -> Assertion
up1Test info lows high' =
    case up1 info lows of
        Left err -> assertFailure (show err)
        Right high -> Pr high @?= Pr high'

upTest
    :: (Name, Domain HasRepresentation Constant)
    -> [(Name, Constant)]
    -> (Name, Constant)
    -> Assertion
upTest info lows high' =
    case up lows info of
        Left err -> assertFailure (show err)
        Right high -> Pr high @?= Pr high'


testCasesAuto
    :: Name                                                      -- high level variable name
    -> Domain HasRepresentation Constant                         -- high level domain
    -> Constant                                                  -- high level value (constant)
    -> [TestTree]
testCasesAuto highName highDomain highConstant =
    [ testCase "downUp1" $ downUp1Test (highName, highDomain, highConstant)
    , testCase "downUp"  $ downUpTest  (highName, highDomain, highConstant)
    ]

downUp1Test
    :: (Name, Domain HasRepresentation Constant, Constant)
    -> Assertion
downUp1Test high =
    case downC1 high of
        Left err -> assertFailure (show err)
        Right mlows -> do
            let lows = maybe [dropDomain high] (map dropDomain) mlows   -- use high if we cannot go downC1
            case up1 (dropConstant high) lows of
                Left err -> assertFailure (show err)
                Right high' -> Pr high' @?= Pr (dropDomain high)

downUpTest
    :: (Name, Domain HasRepresentation Constant, Constant)
    -> Assertion
downUpTest high =
    case downC high of
        Left err -> assertFailure (show err)
        Right lows ->
            case up (map dropDomain lows) (dropConstant high) of
                Left err -> assertFailure (show err)
                Right high' -> Pr high' @?= Pr (dropDomain high)


intDomain :: Int -> Int -> Domain r Constant
intDomain lb ub = anyRepr "RepresentationsTest.intDomain" $ mkDomainIntB (ConstantInt lb) (ConstantInt ub)

dropConstant :: (a,b,c) -> (a,b)
dropConstant (a,b,_) = (a,b)

dropDomain :: (a,b,c) -> (a,c)
dropDomain (a,_,c) = (a,c)


data Pr a = Pr a
    deriving Eq

instance Show (Pr [(Name, Domain HasRepresentation Constant, Constant)]) where
    show (Pr xs) = show $ vcat $ concatMap show' xs
        where
            show' (name, dom, cons) = [ hang (pretty name) 4 $ vcat
                                        [ ":" <+> pretty dom
                                        , "=" <+> pretty cons
                                        ] ]

instance Show (Pr (Maybe [(Name, Domain HasRepresentation Constant, Constant)])) where
    show (Pr Nothing) = "Nothing"
    show (Pr (Just xs)) = show (Pr xs)

instance Show (Pr (Name, Constant)) where
    show (Pr (name, cons)) = show $ pretty name <+> "=" <+> pretty cons

instance Show (Pr [(Name, Constant)]) where
    show (Pr xs) = intercalate "\n" $ map (show . Pr) xs


