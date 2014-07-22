{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Conjure.Language.Definition
    ( module Language.E.Definition

    , Model(..)
    , Statement(..)
    , Objective(..)
    , Declaration(..)
    , Expression(..)
    , ModelInfo(..)
    , Decision(..)
    ) where

-- conjure
import Language.E.Imports
import Language.E.Definition
import Bug

-- base
import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )
import GHC.Show ( showSpace, showList__ )


data Model = Model
    { mStatements :: [Statement]
    , mInfo :: ModelInfo
    }
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

instance Default Model where
    def = Model [] def

data Statement
    = Declaration Declaration
    | Where Expression
    | Objective Objective Expression
    | SuchThat Expression
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

data Objective = Minimising | Maximisinig
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

data Declaration
    = Find    Name (Domain () Expression)
    | Given   Name (Domain () Expression)
    | Letting Name Expression
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

data ModelInfo = ModelInfo
    { miRepresentations :: [(Name, Domain HasRepresentation Expression)]
    , miTrail :: [Decision]
    }
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

instance Default ModelInfo where
    def = ModelInfo [] []

data Decision = Decision
    { dDescription :: Text
    , dOptions :: [Int]
    , dDecision :: Int
    }
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

data Expression
    = Constant Constant
    | Reference Name
    | Op Name [Expression]
    | Lambda Name Type Expression (Expression -> Expression)
    deriving (Data, Typeable, GHC.Generics.Generic)

instance Eq Expression where
    Constant a == Constant b = a == b
    Reference a == Reference b = a == b
    Op n1 xs1 == Op n2 xs2 = n1 == n2 && and (zipWith (==) xs1 xs2)
    Lambda {} == Lambda {} = bug "Lambda's cannot be compared for equality. There you go."
    _ == _ = False

instance Ord Expression where
    Constant  a  `compare` Constant b  = a `compare` b
    Constant  {} `compare` _ = LT
    Reference a  `compare` Reference b = a `compare` b
    Reference {} `compare` _ = LT
    Op nm1 xs1   `compare` Op nm2 xs2 =
        case nm1 `compare` nm2 of
            EQ -> xs1 `compare` xs2
            ow -> ow
    Op {} `compare` _ = LT
    Lambda {} `compare` _ = bug "Lambda's cannot be compared for ordering. There you go."

instance Show Expression where
    showsPrec pr (Constant x      ) = showParen (pr >= 11) (showString "Constant "  . showsPrec 11 x)
    showsPrec pr (Reference x     ) = showParen (pr >= 11) (showString "Reference " . showsPrec 11 x)
    showsPrec pr (Op op xs        ) = showParen (pr >= 11) (showString "Op "        . showsPrec 11 op
                                                                        . showSpace . showsPrec 11 xs)
    showsPrec pr (Lambda nm ty x _) = showParen (pr >= 11) (showString "Lambda "    . showsPrec 11 nm
                                                                        . showSpace . showsPrec 11 ty
                                                                        . showSpace . showsPrec 11 x)
    showList = showList__ (showsPrec 0)

