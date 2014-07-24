module Language.E.Evaluator.ToInt where

import Conjure.Prelude
import Language.E.Definition
import Language.E.CompE

toInt :: MonadConjure m => E -> m (Maybe (Integer, [Binder]))

