module Test.Language.EssenceParsers ( allTests ) where


import Control.Applicative
import Test.HUnit ( Test, (~:), (~=?), test )

import Language.Essence
import Language.EssenceParsers ( pExpr )
import ParsecUtils
import TestUtils ( quickTest )


(~~) :: String -> Expr -> IO Test
s ~~ x = return $   "parsing: " ++ s
                ~:  Just x
                ~=? parseMaybe (pExpr <* eof) s

noParse :: String -> IO Test
noParse s = return $   "no parse: " ++ s
                   ~:  Nothing
                   ~=? parseMaybe (pExpr <* eof) s


allTests :: IO Test
allTests = test <$> sequence

--------------------------------------------------------------------------------
-- Tests for value parsing -----------------------------------------------------
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Tests for parsing domains ---------------------------------------------------
--------------------------------------------------------------------------------

    , "bool" ~~ DomainBoolean

    , "int"        ~~ DomainIntegerList []
    , "int()"      ~~ DomainIntegerList []
    , "int( )"     ~~ DomainIntegerList []
    , "int ( )"    ~~ DomainIntegerList []
    , "int(1)"     ~~ DomainIntegerList [ValueInteger 1]
    , "int(1 )"    ~~ DomainIntegerList [ValueInteger 1]
    , "int( 1)"    ~~ DomainIntegerList [ValueInteger 1]
    , "int( 1 )"   ~~ DomainIntegerList [ValueInteger 1]
    , "int(1,2)"   ~~ DomainIntegerList [ValueInteger 1, ValueInteger 2]
    , "int(1,2,a)" ~~ DomainIntegerList [ValueInteger 1, ValueInteger 2, Identifier "a"]
    , "int(1..)"   ~~ DomainIntegerFromTo (Just (ValueInteger 1)) Nothing
    , "int(1..20)" ~~ DomainIntegerFromTo (Just (ValueInteger 1)) (Just (ValueInteger 20))
    , "int(..20)"  ~~ DomainIntegerFromTo Nothing (Just (ValueInteger 20))
    , noParse "int(1...3)"
    , noParse "int(1,,2,3)"

    , "new type of size 4"     ~~ DomainUnnamed (ValueInteger 4) Nothing
    , "new type of size a"     ~~ DomainUnnamed (Identifier "a") Nothing
    -- , "new type of size a + b" ~~ DomainUnnamed (GenericNode Plus [Identifier "a", Identifier "b"]) Nothing

    , "enum {foo, bar}"      ~~ DomainEnum ["foo", "bar"] Nothing
    , "enum {foo, bar, baz}" ~~ DomainEnum ["foo", "bar", "baz"] Nothing
    , noParse "enum {foo, int}"

    , "matrix indexed by [int(1..9)] of bool"
    ~~ DomainMatrix (DomainIntegerFromTo (Just (ValueInteger 1)) (Just (ValueInteger 9))) DomainBoolean

    , "matrix indexed by [int(1..9)] of matrix indexed by [int(a..b)] of enum {foo, bar}"
    ~~  DomainMatrix (DomainIntegerFromTo (Just (ValueInteger 1)) (Just (ValueInteger 9))) (
            DomainMatrix (DomainIntegerFromTo (Just (Identifier "a")) (Just (Identifier "b"))) (
                DomainEnum ["foo", "bar"] Nothing
            )
        )

    , "matrix indexed by [int(1..9),int(a..b)] of enum {foo, bar}"
    ~~  DomainMatrix (DomainIntegerFromTo (Just (ValueInteger 1)) (Just (ValueInteger 9))) (
            DomainMatrix (DomainIntegerFromTo (Just (Identifier "a")) (Just (Identifier "b"))) (
                DomainEnum ["foo", "bar"] Nothing
            )
        )

    , "matrix indexed by [int(1..9), int(a..), int(..b)] of bool"
    ~~  DomainMatrix (DomainIntegerFromTo (Just (ValueInteger 1)) (Just (ValueInteger 9))) (
            DomainMatrix (DomainIntegerFromTo (Just (Identifier "a")) Nothing) (
                DomainMatrix (DomainIntegerFromTo Nothing (Just (Identifier "b"))) (
                    DomainBoolean
                )
            )
        )

    , "tuple (int,bool)"
    ~~ DomainTuple [DomainIntegerList [], DomainBoolean] Nothing

    , "tuple (int,fool,tuple(enum{foo,bar},int(1..9)))"
    ~~ DomainTuple [ DomainIntegerList [], Identifier "fool"
                   , DomainTuple [ DomainEnum ["foo", "bar"] Nothing
                                 , DomainIntegerFromTo (Just (ValueInteger 1)) (Just (ValueInteger 9))
                                 ] Nothing
                   ] Nothing


    ,  "set of int"
    ~~ DomainSet Nothing Nothing Nothing False (DomainIntegerList []) Nothing

    ,  "set of a"
    ~~ DomainSet Nothing Nothing Nothing False (Identifier "a") Nothing

    ,  "set (size n) of a"
    ~~ DomainSet (Just (Identifier "n")) Nothing Nothing False (Identifier "a") Nothing

    ,  "set (size n) of tuple (a,b)"
    ~~ DomainSet (Just (Identifier "n")) Nothing Nothing False (DomainTuple [Identifier "a",Identifier "b"] Nothing) Nothing

    ,  "set (minSize n) of a"
    ~~ DomainSet Nothing (Just (Identifier "n")) Nothing False (Identifier "a") Nothing

    ,  "set (maxSize n) of a"
    ~~ DomainSet Nothing Nothing (Just (Identifier "n")) False (Identifier "a") Nothing

    ,  "set (representation foo) of a"
    ~~ DomainSet Nothing Nothing Nothing False (Identifier "a") (Just "foo")

    ,  "set (size n, minSize m, representation foo) of a"
    ~~ DomainSet (Just (Identifier "n")) (Just (Identifier "m")) Nothing False (Identifier "a") (Just "foo")

    ,  "set (representation foo) of set of a"
    ~~ DomainSet Nothing Nothing Nothing False (DomainSet Nothing Nothing Nothing False (Identifier "a") Nothing) (Just "foo")

    ,  "set (size n, _) of int"
    ~~ DomainSet (Just (Identifier "n")) Nothing Nothing True (DomainIntegerList []) Nothing

    ,  "set (_, maxSize n) of int"
    ~~ DomainSet Nothing Nothing (Just (Identifier "n")) True (DomainIntegerList []) Nothing

    ,  "set (_, maxSize n, _) of int"
    ~~ DomainSet Nothing Nothing (Just (Identifier "n")) True (DomainIntegerList []) Nothing

    ,  "set (_, maxSize n, _, _) of int"
    ~~ DomainSet Nothing Nothing (Just (Identifier "n")) True (DomainIntegerList []) Nothing


    ,  "mset of int"
    ~~ DomainMSet Nothing Nothing Nothing Nothing Nothing Nothing False (DomainIntegerList []) Nothing

    ,  "mset of a"
    ~~ DomainMSet Nothing Nothing Nothing Nothing Nothing Nothing False (Identifier "a") Nothing

    ,  "mset (size n) of a"
    ~~ DomainMSet (Just (Identifier "n")) Nothing Nothing Nothing Nothing Nothing False (Identifier "a") Nothing

    ,  "mset (size n) of tuple (a,b)"
    ~~ DomainMSet (Just (Identifier "n")) Nothing Nothing Nothing Nothing Nothing False (DomainTuple [Identifier "a",Identifier "b"] Nothing) Nothing

    ,  "mset (minSize n) of a"
    ~~ DomainMSet Nothing (Just (Identifier "n")) Nothing Nothing Nothing Nothing False (Identifier "a") Nothing

    ,  "mset (maxSize n) of a"
    ~~ DomainMSet Nothing Nothing (Just (Identifier "n")) Nothing Nothing Nothing False (Identifier "a") Nothing

    ,  "mset (representation foo) of a"
    ~~ DomainMSet Nothing Nothing Nothing Nothing Nothing Nothing False (Identifier "a") (Just "foo")

    ,  "mset (size n, minSize m, representation foo) of a"
    ~~ DomainMSet (Just (Identifier "n")) (Just (Identifier "m")) Nothing Nothing Nothing Nothing False (Identifier "a") (Just "foo")

    ,  "mset (representation foo) of mset of a"
    ~~ DomainMSet Nothing Nothing Nothing Nothing Nothing Nothing False (DomainMSet Nothing Nothing Nothing Nothing Nothing Nothing False (Identifier "a") Nothing) (Just "foo")

    ,  "mset (size n, _) of int"
    ~~ DomainMSet (Just (Identifier "n")) Nothing Nothing Nothing Nothing Nothing True (DomainIntegerList []) Nothing

    ,  "mset (_, maxSize n) of int"
    ~~ DomainMSet Nothing Nothing (Just (Identifier "n")) Nothing Nothing Nothing True (DomainIntegerList []) Nothing

    ,  "mset (_, maxSize n, _) of int"
    ~~ DomainMSet Nothing Nothing (Just (Identifier "n")) Nothing Nothing Nothing True (DomainIntegerList []) Nothing

    ,  "mset (_, maxSize n, _, _) of int"
    ~~ DomainMSet Nothing Nothing (Just (Identifier "n")) Nothing Nothing Nothing True (DomainIntegerList []) Nothing

    ,  "mset (maxSize n, occr o, _) of int"
    ~~ DomainMSet Nothing Nothing (Just (Identifier "n")) (Just (Identifier "o")) Nothing Nothing True (DomainIntegerList []) Nothing

    ,  "mset (occr o, minOccr m) of int"
    ~~ DomainMSet Nothing Nothing Nothing (Just (Identifier "o")) (Just (Identifier "m")) Nothing False (DomainIntegerList []) Nothing

    ,  "mset (maxOccr m) of int"
    ~~ DomainMSet Nothing Nothing Nothing Nothing Nothing (Just (Identifier "m")) False (DomainIntegerList []) Nothing

    ]
