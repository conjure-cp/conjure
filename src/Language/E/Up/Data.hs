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
          | TagTuple  [[TagT]]
          | TagFunc  [TagT] [TagT]
    deriving (Show)

type IndexRange = E

data IndexT = IndexNone 
            | IndexMatrix IndexRange IndexT
            | IndexTuple [IndexT]
            | IndexRel   [IndexT]
            | IndexFunc   IndexT  IndexT
            | IndexPar    IndexT
            | IndexSet    IndexT
    deriving (Show)

