module Phases.TopLevelAnds where

import Control.Applicative
import Control.Monad.IO.Class ( MonadIO )

import Language.Essence


topLevelAnds :: (Applicative m, MonadIO m) => Spec -> m Spec
topLevelAnds spec = return spec { constraints = concatMap conjunct' (constraints spec) }

conjunct' :: Expr -> [Expr]
conjunct' (GenericNode And [x,y]) = conjunct' x ++ conjunct' y
conjunct' x = [x]
