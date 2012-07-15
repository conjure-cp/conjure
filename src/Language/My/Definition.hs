module Language.My.Definition where

import Stuff.Generic

import Language.Haskell.TH


type My = Generic String Int ()

-- myMatcher :: My -> Q Exp -> Q Exp
-- myMatcher = genMatcher
