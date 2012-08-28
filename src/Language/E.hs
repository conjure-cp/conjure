-- {-# LANGUAGE QuasiQuotes #-}

module Language.E ( module X ) where

import Stuff.CompT as X
import Stuff.Generic as X
import Stuff.NamedLog as X
import Stuff.MonadList as X

import Language.E.Imports as X
import Language.E.Definition as X
import Language.E.Lexer as X
import Language.E.Parser as X
import Language.E.Pretty as X
import Language.E.MatchBind as X
import Language.E.Traversals as X

import Language.E.Evaluator as X
import Language.E.Evaluator.ToBool as X

-- import Language.E.TH ( matchE )
-- import Language.Haskell.TH

-- f [matchE| @a + 0 |] = 12
