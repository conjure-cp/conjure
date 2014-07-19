{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Conjure.Language.Definition
    ( module Language.E.Definition

    , Model(..)
    , Statement(..)
    , Objective(..)
    , Declaration(..)
    , Expression
    , ModelInfo(..)
    , Decision(..)
    ) where

-- conjure
import Language.E.Imports
import Language.E.Definition

-- base
import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )


data Model = Model
    { mStatements :: [Statement]
    , mInfo :: ModelInfo
    }
    deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

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

type Expression = E
-- data Expression
--     = ExpressionConstant Constant
--     | ExpressionReference Name
--     | ExpressionOp Name [Expression]
--     deriving (Eq, Ord, Show, Data, Typeable, GHC.Generics.Generic)

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

