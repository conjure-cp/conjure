module Language.E.Evaluator.ToInt where

import Language.E.Definition
import Language.E.CompE

toInt :: MonadConjure m => E -> m (Maybe (Integer, [Binder]))

