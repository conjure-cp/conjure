{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}

module Language.E.Up.Common(
    isContainer
) where

import Language.E




-- tags that have the same structure as a matrix
isContainer :: Tag -> Bool
isContainer "matrix" = True
isContainer "set"    = True
isContainer "mset"   = True
isContainer _        = False

