module Language.E.Definition.Literal where

import Conjure.Prelude
import Language.E.Definition

data EssenceLiteral
    = ELB Bool
    | ELI Integer
    | ELTuple [EssenceLiteral]
    | ELMatrix [EssenceLiteral] (Maybe E)
    | ELSet [EssenceLiteral]
    | ELMSet [EssenceLiteral]
    | ELFunction [(EssenceLiteral, EssenceLiteral)]   -- list of mappings
    | ELRelation [[EssenceLiteral]]                   -- list of tuples
    | ELPartition [[EssenceLiteral]]                  -- list of parts
    deriving (Eq, Ord, Show)

