
-- This module defines type core data types for Essence.

-- Uses the tool derive, to `derive` instance of type classes.
-- See target `derivations` in the Makefile

{-# LANGUAGE CPP #-}
{-# OPTIONS_DERIVE --output=EssenceDerivations.hs #-}

module Language.Essence ( Spec(..), Expr(..) ) where

import Data.Binary
import Data.Generics


-- the data type for an Essence specification
data Spec
    = Spec { language         :: String
           , version          :: [Int] 
           , topLevelBindings :: [Binding]
           , topLevelWheres   :: [Where]
           , objective        :: Maybe Expr
           , constraints      :: [Expr]
           }
    deriving ({-! Eq, Ord, Read, Show, Binary, Data, Typeable !-})

type Binding = (BindingEnum,Expr,Expr)

data BindingEnum = Find | Given | Letting
    deriving ({-! Eq, Ord, Read, Show, Binary, Data, Typeable !-})

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

    -- Domains
    | DomainBoolean
    | DomainIntegerFromTo (Maybe Expr) (Maybe Expr)
    | DomainIntegerList [Expr]
    | DomainUnnamed
        { theSize        :: Expr
        , representation :: Maybe Representation
        }
    | DomainEnum
        { enums          :: [String]
        , representation :: Maybe Representation
        }
    | DomainMatrix
        { index          :: Expr
        , element        :: Expr
        }
    | DomainTuple
        { components     :: [Expr]
        , representation :: Maybe Representation
        }
    | DomainSet
        { size           :: Maybe Expr
        , minSize        :: Maybe Expr
        , maxSize        :: Maybe Expr
        , attrDontCare   :: Bool
        , element        :: Expr
        , representation :: Maybe Representation
        }
    | DomainMSet
        { size           :: Maybe Expr
        , minSize        :: Maybe Expr
        , maxSize        :: Maybe Expr
        , occr           :: Maybe Expr
        , minOccr        :: Maybe Expr
        , maxOccr        :: Maybe Expr
        , attrDontCare   :: Bool
        , element        :: Expr
        , representation :: Maybe Representation
        }
    | DomainFunction
        { functionFrom   :: Expr
        , functionTo     :: Expr
        , isTotal        :: Bool
        , isPartial      :: Bool
        , isInjective    :: Bool
        , isBijective    :: Bool
        , isSurjective   :: Bool
        , attrDontCare   :: Bool
        , representation :: Maybe Representation
        }
    | DomainRelation
        { components     :: [Expr]
        , representation :: Maybe Representation
        }
    | DomainPartition
        { element        :: Expr
        , isRegular      :: Bool
        , isComplete     :: Bool
        , size           :: Maybe Expr
        , minSize        :: Maybe Expr
        , maxSize        :: Maybe Expr
        , partSize       :: Maybe Expr
        , minPartSize    :: Maybe Expr
        , maxPartSize    :: Maybe Expr
        , numParts       :: Maybe Expr
        , minNumParts    :: Maybe Expr
        , maxNumParts    :: Maybe Expr
        , attrDontCare   :: Bool
        , representation :: Maybe Representation
        }

    | GenericNode Op [Expr]

    deriving ({-! Eq, Ord, Read, Show, Binary, Data, Typeable !-})


type Representation = String


-- the data type for operators in Essence
data Op
    = Plus | Minus | Times | Div | Mod | Pow | Abs | Negate
    | Lt | Leq | Gt | Geq | Neq | Eq
    | Not | Or | And | Imply | Iff
    | Union | Intersect | Subset | SubsetEq | Supset | SupsetEq
    | Card | Elem | Max | Min
    | ToSet | ToMSet | ToRel | Defined | Range
    | Image | PreImage | Inverse
    | Together | Apart
    | Party | Participants | Parts
    | Freq | Hist

    | Index | Project

    | Bubble
    
    | AllDiff

    deriving ({-! Eq, Ord, Read, Show, Binary, Data, Typeable !-})



#include "EssenceDerivations.hs"
