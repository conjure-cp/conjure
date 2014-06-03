{-# LANGUAGE FlexibleContexts #-}

module Conjure.TranslateSolution ( translateSingleSolution, translateSolution ) where

-- conjure
import Conjure.UpDown
import Language.E.Imports
import Language.E.Definition

-- containers
import Data.Tree ( Tree(..) )


-- | Translating a collection of low level values together with their low level domains to high level domains and values.
--   A tree of representations is taken as an argument. Values in this tree give representations for each level of nesting for the domain.
--   Example: NoRepresentation (x, int, 3) --> (x, int, 3)
--
--            Explicit         (x_Explicit, matrix indexed by [int(1..4)] of int(1..9), [1,3,5,6;int(1..4)]) --> (x,set (size 4) of int(1..9),{1,3,5,6})
--
--            NoRepresentation (x_1, int, 1) (x_2, bool, false) --> (x, (int,bool), (1,false))
--
--            Explicit, Explicit
--                ( x_Explicit_Explicit
--                , matrix indexed by [int(1..4)] of matrix indexed by [int(1..3)] of int(1..9)
--                , [ [1,3,5;int(1..3)]
--                  , [2,3,6;int(1..3)]
--                  , [3,4,5;int(1..3)]
--                  , [3,4,8;int(1..3)]
--                  ; int(1..4)
--                  ]
--                )
--            --> ( x_Explicit
--                , matrix indexed by [int(1..4)] of set (size 3) of int(1..9)
--                , [ {1,3,5}
--                  , {2,3,6}
--                  , {3,4,5}
--                  , {3,4,8}
--                  ; int(1..4)
--                  ]
--                )
--            --> ( x
--                , set (size 4) of set (size 3) of int(1..9)
--                , { {1,3,5}
--                  , {2,3,6}
--                  , {3,4,5}
--                  , {3,4,8}
--                  }
--                )
--
--            Explicit, Occurrence
--                ( x_Explicit_Occurrence
--                , matrix indexed by [int(1..4)] of matrix indexed by [int(1..9)] of bool
--                , [ [1,0,1,0,1,0,0,0,0;int(1..9)]
--                  , [0,1,1,0,0,1,0,0,0;int(1..9)]
--                  , [0,0,1,1,1,0,0,0,0;int(1..9)]
--                  , [0,0,1,1,0,0,0,1,0;int(1..9)]
--                  ; int(1..4)
--                  ]
--                )
--            --> ( x_Explicit
--                , matrix indexed by [int(1..4)] of set (size 3) of int(1..9)
--                , [ {1,3,5}
--                  , {2,3,6}
--                  , {3,4,5}
--                  , {3,4,8}
--                  ; int(1..4)
--                  ]
--                )
--            --> ( x
--                , set (size 4) of set (size 3) of int(1..9)
--                , { {1,3,5}
--                  , {2,3,6}
--                  , {3,4,5}
--                  , {3,4,8}
--                  }
--                )
-- 
translateSingleSolution
    :: MonadError UpDownError m
    => Tree Representation
    ->  [(Text, Domain Constant, Constant)]
    -> m (Text, Domain Constant, Constant)
translateSingleSolution = error "translateSingleSolution"


translateSolution
    :: MonadError UpDownError m
    => Spec                         -- ^ Essence
    -> Spec                         -- ^ Essence'
    -> Spec                         -- ^ Essence' Solution
    -> m Spec                       -- ^ Essence Solution
translateSolution = error "translateSolution"

