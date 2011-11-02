
-- This module defines core data types for Essence.

-- Uses the tool derive, to `derive` instance of type classes.
-- See target `derivations` in the Makefile

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_DERIVE --output=EssenceDerivations.hs #-}

module Language.Essence ( Spec(..), Expr(..), Op(..), OpDescriptor(..), opDescriptor ) where

--------------------------------------------------------------------------------
-- imports ---------------------------------------------------------------------
--------------------------------------------------------------------------------

import Data.Binary
import Data.Generics.Uniplate.Direct


--------------------------------------------------------------------------------
-- Log -------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- any kind of log generated while running Conjure is of this data type
type Log = String


--------------------------------------------------------------------------------
-- Spec ------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- the data type for an Essence specification
data Spec
    = Spec { language         :: String
           , version          :: [Int] 
           , topLevelBindings :: [Binding]
           , topLevelWheres   :: [Where]
           , objective        :: Maybe Expr
           , constraints      :: [Expr]
           }
    deriving (Eq, Ord, Read, Show)

type Binding = (BindingEnum,Expr,Expr)

data BindingEnum = Find | Given | Letting
    deriving (Eq, Ord, Read, Show)

type Where = Expr


--------------------------------------------------------------------------------
-- Expr ------------------------------------------------------------------------
--------------------------------------------------------------------------------

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
        , attrDontCare   :: Bool
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

    deriving (Eq, Ord, Read, Show)


type Representation = String


--------------------------------------------------------------------------------
-- Op --------------------------------------------------------------------------
--------------------------------------------------------------------------------

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

    deriving (Eq, Ord, Read, Show, Enum, Bounded)


-- will be used while parsing and pretty-printing operators
data OpDescriptor
    = OpLispy  { face :: String, cardinality :: Int }
    | OpInfixL { face :: String, precedence  :: Int }
    | OpInfixN { face :: String, precedence  :: Int }
    | OpInfixR { face :: String, precedence  :: Int }
    | OpPrefix { face :: String, precedence  :: Int }
    | OpSpecial

opDescriptor :: Op -> OpDescriptor
opDescriptor Plus         = OpInfixL "+"            3
opDescriptor Minus        = OpInfixL "-"            3
opDescriptor Times        = OpInfixL "*"            4
opDescriptor Div          = OpInfixL "/"            4
opDescriptor Mod          = OpInfixL "%"            4
opDescriptor Pow          = OpInfixR "^"            5
opDescriptor Abs          = OpLispy  "abs"          1
opDescriptor Negate       = OpPrefix "-"            10
opDescriptor Lt           = OpInfixN "<"            2
opDescriptor Leq          = OpInfixN "<="           2
opDescriptor Gt           = OpInfixN ">"            2
opDescriptor Geq          = OpInfixN ">="           2
opDescriptor Neq          = OpInfixN "!="           2
opDescriptor Eq           = OpInfixN "="            2
opDescriptor Not          = OpPrefix "!"            10
opDescriptor Or           = OpInfixL "\\/"          1
opDescriptor And          = OpInfixL "/\\"          1
opDescriptor Imply        = OpInfixN "=>"           1
opDescriptor Iff          = OpInfixN "<=>"          1
opDescriptor Union        = OpInfixL "union"        3
opDescriptor Intersect    = OpInfixL "intersect"    3
opDescriptor Subset       = OpInfixN "subset"       2
opDescriptor SubsetEq     = OpInfixN "subseteq"     2
opDescriptor Supset       = OpInfixN "supset"       2
opDescriptor SupsetEq     = OpInfixN "supseteq"     2
opDescriptor Card         = OpLispy  "card"         1
opDescriptor Elem         = OpInfixN "elem"         2
opDescriptor Max          = OpLispy  "max"          1
opDescriptor Min          = OpLispy  "min"          1
opDescriptor ToSet        = OpLispy  "toSet"        1
opDescriptor ToMSet       = OpLispy  "toMSet"       1
opDescriptor ToRel        = OpLispy  "toRel"        1
opDescriptor Defined      = OpLispy  "defined"      1
opDescriptor Range        = OpLispy  "range"        1
opDescriptor Image        = OpLispy  "image"        2
opDescriptor PreImage     = OpLispy  "preimage"     2
opDescriptor Inverse      = OpLispy  "inverse"      2
opDescriptor Together     = OpLispy  "together"     3
opDescriptor Apart        = OpLispy  "apart"        3
opDescriptor Party        = OpLispy  "party"        2
opDescriptor Participants = OpLispy  "participants" 1
opDescriptor Parts        = OpLispy  "parts"        1
opDescriptor Freq         = OpLispy  "freq"         2
opDescriptor Hist         = OpLispy  "hist"         2
opDescriptor Project      = OpSpecial
opDescriptor Index        = OpSpecial
opDescriptor Bubble       = OpInfixN "@"            2
opDescriptor AllDiff      = OpLispy  "alldifferent" 1


--------------------------------------------------------------------------------
-- type class instances --------------------------------------------------------
--------------------------------------------------------------------------------

{-!

deriving instance Binary Spec
deriving instance Binary Expr
deriving instance Binary Op
deriving instance Binary BindingEnum

deriving instance UniplateDirect Spec Expr
deriving instance UniplateDirect Expr
deriving instance UniplateDirect [Expr] Expr
deriving instance UniplateDirect (Maybe Expr) Expr
deriving instance UniplateDirect (Expr, Expr) Expr
deriving instance UniplateDirect (BindingEnum,Expr,Expr) Expr

!-}

#include "EssenceDerivations.hs"
