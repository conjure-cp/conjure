
-- This module defines type core data types for Essence.

-- Uses the tool derive, to `derive` instance of type classes.
-- See target `derivations` in the Makefile

{-# LANGUAGE CPP #-}
{-# OPTIONS_DERIVE --output=EssenceDerivations.hs #-}

module Language.Essence ( Spec(..), Expr(..) ) where

import Data.Binary
import Data.Generics.Uniplate.Direct


-- let's just use strings for now.
type Op = String


-- the data type for an Essence specification
data Spec
    = Spec { language         :: String
           , version          :: [Int] 
           , topLevelBindings :: [Binding]
           , topLevelWheres   :: [Where]
           , objective        :: Maybe Expr
           , constraints      :: [Expr]
           }
    deriving ({-! Eq, Ord, Read, Show, Binary, UniplateDirect !-})

type Binding = (BindingEnum,Expr,Expr)

data BindingEnum = Find | Given | Letting
    deriving ({-! Eq, Ord, Read, Show, Binary, UniplateDirect !-})

type Where = Expr


-- the data type for any kind of expression in Essence
-- this will end up being a giant type, but decidedly so.
data Expr
    = Identifier String

    -- Inline values
    | ValueBoolean   Bool
    | ValueInteger   Integer
    | ValueMatrix    [Expr]         -- the list has to be of uniform type.
    | ValueTuple     [Expr]
    | ValueSet       [Expr]         -- the list has to be unique.
    | ValueMSet      [Expr]
    | ValueFunction  [(Expr,Expr)]  -- the list (both fsts and snds) has to be of uniform type.
    | ValueRelation  [Expr]         -- has to be a list of tuples of uniform type.
    | ValuePartition [[Expr]]       -- has to be a list of lists of uniform type.

    | GenericNode Op [Expr]

    deriving ({-! Eq, Ord, Read, Show, Binary, UniplateDirect !-})


#include "EssenceDerivations.hs"
