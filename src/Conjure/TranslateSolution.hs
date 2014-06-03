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
--            Explicit         (x_Explicit, matrix indexed by [int(1..4)] of int(1..9), [1,3,5;int(1..9)]) --> (x,set (size 4) of int(1..9),{1,3,5})
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

