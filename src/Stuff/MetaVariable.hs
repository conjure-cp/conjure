module Stuff.MetaVariable where

import Conjure.Prelude

class MetaVariable a where
    unnamedMV :: a -> Bool
    namedMV   :: a -> Maybe Text

