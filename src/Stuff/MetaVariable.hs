module Stuff.MetaVariable where

class MetaVariable a where
    unnamedMV :: a -> Bool
    namedMV   :: a -> Maybe String
