
-- This module defines core data types for Essence.

-- Uses the tool derive, to `derive` instance of type classes.
-- See target `derivations` in the Makefile

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_DERIVE --output=EssenceDerivations.hs #-}

module Language.Essence
    ( Log
    , Spec(..), Binding, BindingEnum(..), Where
    , Objective, ObjectiveEnum(..)
    , Expr(..), Op(..), OpDescriptor(..), opDescriptor
    , Kind(..), Type(..)
    , associativeOps, commutativeOps, validOpTypes
    ) where


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
           , objective        :: Maybe Objective
           , constraints      :: [Expr]
           }
    deriving (Eq, Ord, Read, Show)

type Binding = (BindingEnum,String,Expr)

data BindingEnum = Find | Given | Letting
    deriving (Eq, Ord, Read, Show)

type Objective = (ObjectiveEnum,Expr)

data ObjectiveEnum = Minimising | Maximising
    deriving (Eq, Ord, Read, Show)

type Where = Expr


--------------------------------------------------------------------------------
-- Expr ------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- the data type for any kind of expression in Essence
-- this will end up being a giant type, but decidedly so.
data Expr
    = Underscore

    | GenericNode Op [Expr]

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

    | Identifier String

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
opDescriptor Plus         = OpInfixL "+"            $ 10000 - 400
opDescriptor Minus        = OpInfixL "-"            $ 10000 - 400
opDescriptor Times        = OpInfixL "*"            $ 10000 - 300
opDescriptor Div          = OpInfixL "/"            $ 10000 - 300
opDescriptor Mod          = OpInfixL "%"            $ 10000 - 300
opDescriptor Pow          = OpInfixR "^"            $ 10000 - 200
opDescriptor Abs          = OpLispy  "abs"          1
opDescriptor Negate       = OpPrefix "-"            $ 10000 - 100
opDescriptor Lt           = OpInfixN "<"            $ 10000 - 800
opDescriptor Leq          = OpInfixN "<="           $ 10000 - 800
opDescriptor Gt           = OpInfixN ">"            $ 10000 - 800
opDescriptor Geq          = OpInfixN ">="           $ 10000 - 800
opDescriptor Neq          = OpInfixN "!="           $ 10000 - 800
opDescriptor Eq           = OpInfixN "="            $ 10000 - 800
opDescriptor Not          = OpPrefix "!"            $ 10000 - 1300
opDescriptor Or           = OpInfixL "\\/"          $ 10000 - 1000
opDescriptor And          = OpInfixL "/\\"          $ 10000 - 900
opDescriptor Imply        = OpInfixN "=>"           $ 10000 - 1100
opDescriptor Iff          = OpInfixN "<=>"          $ 10000 - 1200
opDescriptor Union        = OpInfixL "union"        $ 10000 - 300
opDescriptor Intersect    = OpInfixL "intersect"    $ 10000 - 300
opDescriptor Subset       = OpInfixN "subset"       $ 10000 - 700
opDescriptor SubsetEq     = OpInfixN "subseteq"     $ 10000 - 700
opDescriptor Supset       = OpInfixN "supset"       $ 10000 - 700
opDescriptor SupsetEq     = OpInfixN "supseteq"     $ 10000 - 700
opDescriptor Card         = OpLispy  "card"         1
opDescriptor Elem         = OpInfixN "elem"         $ 10000 - 700
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
opDescriptor Bubble       = OpInfixN "@"            $ 10000 - 0
opDescriptor AllDiff      = OpLispy  "alldifferent" 1


associativeOps :: [Op]
associativeOps = [Plus,Times,And,Or]

commutativeOps :: [Op]
commutativeOps = [Plus,Times,And,Or,Eq,Neq,Iff]


--------------------------------------------------------------------------------
-- Kinds and Types -------------------------------------------------------------
--------------------------------------------------------------------------------

data Kind = KindUnknown | KindDomain | KindValue | KindExpr
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

data Type = TypeUnknown
    | TypeIdentifier String
    | TypeBoolean
    | TypeInteger
    | TypeUnnamed
    | TypeEnum
    | TypeMatrix Type
    | TypeTuple [Type]
    | TypeSet Type
    | TypeMSet Type
    | TypeFunction Type Type
    | TypeRelation [Type]
    | TypePartition Type
    deriving (Eq, Ord, Read, Show)

class TypeUnify t where
    typeUnify :: t -> t -> Bool
    chooseType :: t -> t -> t

instance TypeUnify Type where
    typeUnify TypeUnknown _ = True
    typeUnify _ TypeUnknown = True
    typeUnify a b = a == b
    chooseType t TypeUnknown = t
    chooseType TypeUnknown t = t
    chooseType t _ = t

instance TypeUnify t => TypeUnify [t] where
    typeUnify as bs = and $ zipWith typeUnify as bs
    chooseType as bs = zipWith chooseType as bs

instance (TypeUnify t1, TypeUnify t2) => TypeUnify (t1,t2) where
    typeUnify (a,b) (c,d) = typeUnify a c && typeUnify b d
    chooseType (a,b) (c,d) = (chooseType a c, chooseType b d)

(~~) :: TypeUnify t => t -> t -> Bool
(~~) = typeUnify


-- All the ops in Essence, with their overloadings if they are polymorphic
validOpTypes :: Op -> [Type] -> Maybe Type
validOpTypes Plus  ps@[_,_] | all (`elem` [TypeBoolean,TypeInteger]) ps = return TypeInteger
validOpTypes Minus ps@[_,_] | all (`elem` [TypeBoolean,TypeInteger]) ps = return TypeInteger
validOpTypes Times ps@[_,_] | all (`elem` [TypeBoolean,TypeInteger]) ps = return TypeInteger
validOpTypes Div   ps@[_,_] | all (`elem` [TypeBoolean,TypeInteger]) ps = return TypeInteger
validOpTypes Mod   ps@[_,_] | all (`elem` [TypeBoolean,TypeInteger]) ps = return TypeInteger
validOpTypes Pow   ps@[_,_] | all (`elem` [TypeBoolean,TypeInteger]) ps = return TypeInteger
validOpTypes Abs    [TypeBoolean] = return TypeInteger
validOpTypes Abs    [TypeInteger] = return TypeInteger
validOpTypes Negate [TypeInteger] = return TypeInteger

validOpTypes Lt  ps@[_,_] | all (`elem` [TypeBoolean,TypeInteger]) ps = return TypeBoolean
validOpTypes Leq ps@[_,_] | all (`elem` [TypeBoolean,TypeInteger]) ps = return TypeBoolean
validOpTypes Gt  ps@[_,_] | all (`elem` [TypeBoolean,TypeInteger]) ps = return TypeBoolean
validOpTypes Geq ps@[_,_] | all (`elem` [TypeBoolean,TypeInteger]) ps = return TypeBoolean
validOpTypes Neq ps@[_,_] | all (`elem` [TypeBoolean,TypeInteger]) ps = return TypeBoolean
validOpTypes Eq  ps@[_,_] | all (`elem` [TypeBoolean,TypeInteger]) ps = return TypeBoolean
validOpTypes Neq [a,b] | a == b = return TypeBoolean
validOpTypes Eq  [a,b] | a == b = return TypeBoolean

validOpTypes Not   [TypeBoolean]             = return TypeBoolean
validOpTypes Or    [TypeBoolean,TypeBoolean] = return TypeBoolean
validOpTypes And   [TypeBoolean,TypeBoolean] = return TypeBoolean
validOpTypes Imply [TypeBoolean,TypeBoolean] = return TypeBoolean
validOpTypes Iff   [TypeBoolean,TypeBoolean] = return TypeBoolean

validOpTypes Union     [TypeSet        a, TypeSet        b] | a  ~~ b  = return $ TypeSet      $ chooseType a b
validOpTypes Union     [TypeMSet       a, TypeMSet       b] | a  ~~ b  = return $ TypeMSet     $ chooseType a b
validOpTypes Union     [TypeRelation  as, TypeRelation  bs] | as ~~ bs = return $ TypeRelation $ chooseType as bs

validOpTypes Intersect [TypeSet        a, TypeSet        b] | a  ~~ b  = return $ TypeSet      $ chooseType a b
validOpTypes Intersect [TypeMSet       a, TypeMSet       b] | a  ~~ b  = return $ TypeMSet     $ chooseType a b
validOpTypes Intersect [TypeRelation  as, TypeRelation  bs] | as ~~ bs = return $ TypeRelation $ chooseType as bs
validOpTypes Intersect [TypeFunction a b, TypeFunction c d] | (a,b) ~~ (c,d) = return $ TypeFunction (chooseType a c) (chooseType c d)

validOpTypes Minus     [TypeSet        a, TypeSet        b] | a  ~~ b  = return $ TypeSet      $ chooseType a b
validOpTypes Minus     [TypeMSet       a, TypeMSet       b] | a  ~~ b  = return $ TypeMSet     $ chooseType a b
validOpTypes Minus     [TypeRelation  as, TypeRelation  bs] | as ~~ bs = return $ TypeRelation $ chooseType as bs
validOpTypes Minus     [TypeFunction a b, TypeFunction c d] | (a,b) ~~ (c,d) = return $ TypeFunction (chooseType a c) (chooseType c d)

-- even though there is a trivial code duplication here (which can be avoided
-- using pattern guards), I am happy to have it because it helps the
-- completeness checker. This function needs to be total, on the set of
-- defined operators.
validOpTypes Subset    [TypeSet        a, TypeSet        b] | a  ~~ b  = return TypeBoolean
validOpTypes Subset    [TypeMSet       a, TypeMSet       b] | a  ~~ b  = return TypeBoolean
validOpTypes Subset    [TypeRelation  as, TypeRelation  bs] | as ~~ bs = return TypeBoolean
validOpTypes Subset    [TypeFunction a b, TypeFunction c d] | (a,b) ~~ (c,d) = return TypeBoolean

validOpTypes SubsetEq  [TypeSet        a, TypeSet        b] | a  ~~ b  = return TypeBoolean
validOpTypes SubsetEq  [TypeMSet       a, TypeMSet       b] | a  ~~ b  = return TypeBoolean
validOpTypes SubsetEq  [TypeRelation  as, TypeRelation  bs] | as ~~ bs = return TypeBoolean
validOpTypes SubsetEq  [TypeFunction a b, TypeFunction c d] | (a,b) ~~ (c,d) = return TypeBoolean

validOpTypes Supset    [TypeSet        a, TypeSet        b] | a  ~~ b  = return TypeBoolean
validOpTypes Supset    [TypeMSet       a, TypeMSet       b] | a  ~~ b  = return TypeBoolean
validOpTypes Supset    [TypeRelation  as, TypeRelation  bs] | as ~~ bs = return TypeBoolean
validOpTypes Supset    [TypeFunction a b, TypeFunction c d] | (a,b) ~~ (c,d) = return TypeBoolean

validOpTypes SupsetEq  [TypeSet        a, TypeSet        b] | a  ~~ b  = return TypeBoolean
validOpTypes SupsetEq  [TypeMSet       a, TypeMSet       b] | a  ~~ b  = return TypeBoolean
validOpTypes SupsetEq  [TypeRelation  as, TypeRelation  bs] | as ~~ bs = return TypeBoolean
validOpTypes SupsetEq  [TypeFunction a b, TypeFunction c d] | (a,b) ~~ (c,d) = return TypeBoolean

validOpTypes Card      [TypeSet       {}] = return TypeInteger
validOpTypes Card      [TypeMSet      {}] = return TypeInteger
validOpTypes Card      [TypeRelation  {}] = return TypeInteger
validOpTypes Card      [TypeFunction  {}] = return TypeInteger
validOpTypes Card      [TypePartition {}] = return TypeInteger

validOpTypes Elem      [_, TypeSet TypeUnknown] = return TypeBoolean
validOpTypes Elem      [a, TypeSet b ] | a == b = return TypeBoolean
validOpTypes Elem      [_, TypeMSet TypeUnknown] = return TypeBoolean
validOpTypes Elem      [a, TypeMSet b ] | a == b = return TypeBoolean
validOpTypes Elem      [TypeTuple as, TypeRelation bs] | as == bs = return TypeBoolean

validOpTypes Max       [TypeSet TypeBoolean   ] = return TypeBoolean
validOpTypes Max       [TypeSet TypeInteger] = return TypeInteger
validOpTypes Min       [TypeSet TypeBoolean   ] = return TypeBoolean
validOpTypes Min       [TypeSet TypeInteger] = return TypeInteger

validOpTypes ToSet     [TypeMSet       a] = return (TypeSet a)
validOpTypes ToSet     [TypeRelation  as] = return (TypeSet (TypeTuple as))
validOpTypes ToSet     [TypeFunction a b] = return (TypeSet (TypeTuple [a,b]))

validOpTypes ToMSet    [TypeSet        a] = return (TypeMSet a)
validOpTypes ToMSet    [TypeRelation  as] = return (TypeMSet (TypeTuple as))
validOpTypes ToMSet    [TypeFunction a b] = return (TypeMSet (TypeTuple [a,b]))

validOpTypes ToRel     [TypeFunction a b] = return (TypeRelation [a,b])

validOpTypes Defined   [TypeFunction a _] = return (TypeSet a)
validOpTypes Range     [TypeFunction _ b] = return (TypeSet b)

validOpTypes Image     [TypeFunction a b,c] | a == c = return b
validOpTypes PreImage  [TypeFunction a b,c] | b == c = return (TypeSet a)

validOpTypes Image     [TypeRelation as,TypeTuple bs] | as == bs = return TypeBoolean

validOpTypes Inverse   [TypeFunction a b, TypeFunction c d] | (a,b) == (d,c) = return TypeBoolean

validOpTypes Together  [a,b,TypePartition c] | a == b && a == c = return TypeBoolean
validOpTypes Apart     [a,b,TypePartition c] | a == b && a == c = return TypeBoolean

validOpTypes Party     [a,TypePartition b] | a == b = return (TypeSet a)
validOpTypes Participants [TypePartition a] = return (TypeSet a)

validOpTypes Parts     [TypePartition a] = return (TypeSet (TypeSet a))

validOpTypes Freq      [TypeMSet a, b] | a == b = return TypeInteger
validOpTypes Hist      [TypeMSet a,TypeMatrix b] | a == b = return (TypeMatrix TypeInteger)

validOpTypes Bubble    [a, TypeBoolean] = return a

validOpTypes AllDiff   [TypeMatrix {}] = return TypeBoolean

validOpTypes _ _ = Nothing



--------------------------------------------------------------------------------
-- type class instances --------------------------------------------------------
--------------------------------------------------------------------------------

{-!

deriving instance Binary Spec
deriving instance Binary Expr
deriving instance Binary Op
deriving instance Binary BindingEnum
deriving instance Binary ObjectiveEnum

deriving instance UniplateDirect Spec Expr
deriving instance UniplateDirect Expr Expr
deriving instance UniplateDirect Expr
deriving instance UniplateDirect [Expr] Expr
deriving instance UniplateDirect (Maybe Expr) Expr
deriving instance UniplateDirect (Expr, Expr) Expr
deriving instance UniplateDirect (BindingEnum,String,Expr) Expr
deriving instance UniplateDirect (ObjectiveEnum,Expr) Expr
deriving instance UniplateDirect (Maybe Objective) Expr

!-}

#include "EssenceDerivations.hs"
