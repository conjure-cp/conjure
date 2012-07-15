{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.E.Test where

import Stuff.Generic
import Stuff.Pretty

import Text.Show.Pretty

-- test [xMatch| attrs   := domain.set.attributes
--            |]
--     = ppShow attrs
-- test [xMatch| [inner] := domain.set.inner
--            |]
--     = ppShow inner
test [xMatch| attrs   := domain.set.attributes
           | [inner] := domain.set.inner
           |]
    = ppShow (attrs, inner)
test _ = "catch all"

-- viewTaggeds [["domain", "set", "attributes"], ["domain", "set", "inner"]]
