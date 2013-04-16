module Language.E.Definition.Literal where

data EssenceLiteral
    = ELB Bool
    | ELI Integer
    | ELTuple [EssenceLiteral]
    | ELMatrix [EssenceLiteral]
    | ELSet [EssenceLiteral]
    | ELMSet [EssenceLiteral]
    | ELFunction [(EssenceLiteral, EssenceLiteral)]   -- list of mappings
    | ELRelation [[EssenceLiteral]]                   -- list of tuples
    | ELPartition [[EssenceLiteral]]                  -- list of parts
    deriving (Eq, Ord, Show)

