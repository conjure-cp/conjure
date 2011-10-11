
-- This module defines type core data types for Essence.

-- Uses the tool derive, to `derive` instance of type classes.
-- See target `derivations` in the Makefile

{-# LANGUAGE CPP #-}
{-# OPTIONS_DERIVE --output=EssenceDerivations.hs #-}

module Language.Essence ( Expr(..) ) where

import Data.Binary
import Data.Generics.Uniplate.Direct


-- let's just use strings for now.
type Op = String


-- a type for any kind of expression in Essence
-- this will end up being a giant type, but decidedly so.
data Expr
    = Identifier String
    | GenericNode Op [Expr]

    deriving ({-! Eq, Ord, Read, Show, Binary, UniplateDirect !-})


#include "EssenceDerivations.hs"
