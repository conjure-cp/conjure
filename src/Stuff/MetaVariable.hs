module Stuff.MetaVariable where

import Data.Text ( Text )

class MetaVariable a where
    unnamedMV :: a -> Bool
    namedMV   :: a -> Maybe Text

