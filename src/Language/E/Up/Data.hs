module Language.E.Up.Data where

import Language.E


data VarInfo =  VarInfo {
    indexes :: [[Integer]],
    bounds  :: [Integer]
} deriving (Show)


data VarData =  VarData {
    vIndexes :: [[Integer]],
    vBounds  :: [Integer],
    vEssence :: E
} deriving (Show)

data Tree a = Leaf a
            | Branch a [Tree a]
            | Tuple [Tree a]
    deriving (Show)


data TagT = TagSingle Tag
          | TagEnum   String
          | TagUnamed String
          | TagTuple [[TagT]]
    deriving (Show)


