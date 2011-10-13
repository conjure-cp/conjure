module Test.Language.EssenceParsers ( allTests ) where


import Control.Applicative
import Test.HUnit ( Test, (~:), (~=?), test )

import Language.Essence
import Language.EssenceParsers ( pExpr )
import ParsecUtils
import TestUtils ( quickTest )


(~~) :: String -> Expr -> IO Test
s ~~ x = return $   "parsing: " ++ s
                ~:  parseMaybe pExpr s
                ~=? Just x


allTests :: IO Test
allTests = test <$> sequence
    [ "false"
    ~~ ValueBoolean False

    , "true"
    ~~ ValueBoolean True

    , "1"
    ~~ ValueInteger 1

    , quickTest "integer literals" $ \ i -> let j = abs i in Just (ValueInteger j) == parseMaybe pExpr (show j)

    , "[1,2,3]"
    ~~ ValueMatrix [ ValueInteger 1
                   , ValueInteger 2
                   , ValueInteger 3
                   ]

    , "[1,2,3,false,4]"
    ~~ ValueMatrix [ ValueInteger 1
                   , ValueInteger 2
                   , ValueInteger 3
                   , ValueBoolean False
                   , ValueInteger 4
                   ]

    , "[]"
    ~~ ValueMatrix []

    , "[[]]"
    ~~ ValueMatrix [ValueMatrix []]

    , "[[1,2,3],[4,5,6]]"
    ~~ ValueMatrix [ ValueMatrix [ValueInteger 1, ValueInteger 2, ValueInteger 3]
                   , ValueMatrix [ValueInteger 4, ValueInteger 5, ValueInteger 6]
                   ]

    , "[[1,2,3],[true,false]]"
    ~~ ValueMatrix [ ValueMatrix [ValueInteger 1, ValueInteger 2, ValueInteger 3]
                   , ValueMatrix [ValueBoolean True, ValueBoolean False]
                   ]

    , "(1,2)"
    ~~ ValueTuple [ValueInteger 1, ValueInteger 2]

    , "(1,2,3,[1,2,3])"
    ~~ ValueTuple [ ValueInteger 1
                  , ValueInteger 2
                  , ValueInteger 3
                  , ValueMatrix [ValueInteger 1, ValueInteger 2, ValueInteger 3]
                  ]

    , "(true,1,(false,2))"
    ~~ ValueTuple [ ValueBoolean True
                  , ValueInteger 1
                  , ValueTuple [ValueBoolean False, ValueInteger 2]
                  ]

    , "set {}"
    ~~ ValueSet []

    , "set {1}"
    ~~ ValueSet [ValueInteger 1]

    , "set {a}"
    ~~ ValueSet [Identifier "a"]

    , "set {1,2,true,false,(1,2,3)}"
    ~~ ValueSet [ ValueInteger 1
                , ValueInteger 2
                , ValueBoolean True
                , ValueBoolean False
                , ValueTuple [ ValueInteger 1
                             , ValueInteger 2
                             , ValueInteger 3
                             ]
                ]

    , "set {set {1,2,3}, set {1,3,5}, set {2,4,6}}"
    ~~ ValueSet [ ValueSet [ValueInteger 1, ValueInteger 2, ValueInteger 3]
                , ValueSet [ValueInteger 1, ValueInteger 3, ValueInteger 5]
                , ValueSet [ValueInteger 2, ValueInteger 4, ValueInteger 6]
                ]

    , "mset {}"
    ~~ ValueMSet []

    , "mset {1}"
    ~~ ValueMSet [ValueInteger 1]

    , "mset {a}"
    ~~ ValueMSet [Identifier "a"]

    , "mset{mset{}}"
    ~~ ValueMSet [ValueMSet []]

    , "mset {1,2,true,false,(1,2,3)}"
    ~~ ValueMSet [ ValueInteger 1
                 , ValueInteger 2
                 , ValueBoolean True
                 , ValueBoolean False
                 , ValueTuple [ ValueInteger 1
                              , ValueInteger 2
                              , ValueInteger 3
                              ]
                 ]

    , "mset {set {1,2,3}, set {1,3,5}, set {2,4,6}}"
    ~~ ValueMSet [ ValueSet [ValueInteger 1, ValueInteger 2, ValueInteger 3]
                 , ValueSet [ValueInteger 1, ValueInteger 3, ValueInteger 5]
                 , ValueSet [ValueInteger 2, ValueInteger 4, ValueInteger 6]
                 ]

    , "function {}"
    ~~ ValueFunction []

    , "function {1->2}"
    ~~ ValueFunction [(ValueInteger 1, ValueInteger 2)]

    , "function {1->2,3->4 , 5 ->6, 7-> 8}"
    ~~ ValueFunction [ (ValueInteger 1, ValueInteger 2)
                     , (ValueInteger 3, ValueInteger 4)
                     , (ValueInteger 5, ValueInteger 6)
                     , (ValueInteger 7, ValueInteger 8)
                     ]

    , "function {1->set{2},3->mset{4} , 5 ->function {6->6}, 7-> (false,true,4)}"
    ~~ ValueFunction [ (ValueInteger 1, ValueSet [ValueInteger 2])
                     , (ValueInteger 3, ValueMSet [ValueInteger 4])
                     , (ValueInteger 5, ValueFunction [(ValueInteger 6, ValueInteger 6)])
                     , (ValueInteger 7, ValueTuple [ValueBoolean False, ValueBoolean True, ValueInteger 4])
                     ]

    , "relation {}"
    ~~ ValueRelation []

    , "relation {(1,a)}"
    ~~ ValueRelation [ValueTuple [ValueInteger 1, Identifier "a"]]

    , "relation { (1,a), (2,b) }"
    ~~ ValueRelation [ ValueTuple [ValueInteger 1, Identifier "a"]
                     , ValueTuple [ValueInteger 2, Identifier "b"]
                     ]

    , "relation { (1,set {a}), (mset {2,3,4},b) }"
    ~~ ValueRelation [ ValueTuple [ValueInteger 1, ValueSet [Identifier "a"]]
                     , ValueTuple [ValueMSet [ValueInteger 2, ValueInteger 3, ValueInteger 4], Identifier "b"]
                     ]

    , "partition {}"
    ~~ ValuePartition []

    , "partition {{},{1},{2},{3},{1,2},{1,3},{2,3},{1,2,3}}"
    ~~ ValuePartition [ [ ]
                      , [ ValueInteger 1 ]
                      , [ ValueInteger 2 ]
                      , [ ValueInteger 3 ]
                      , [ ValueInteger 1, ValueInteger 2 ]
                      , [ ValueInteger 1, ValueInteger 3 ]
                      , [ ValueInteger 2, ValueInteger 3 ]
                      , [ ValueInteger 1, ValueInteger 2, ValueInteger 3 ]
                      ]

    , "partition {{1,2,3},{4,5,6}}"
    ~~ ValuePartition [ [ ValueInteger 1, ValueInteger 2, ValueInteger 3 ]
                      , [ ValueInteger 4, ValueInteger 5, ValueInteger 6 ]
                      ]

    ]
