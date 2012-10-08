module Language.E.Evaluator.ToInt where

import Language.E.Definition
import Language.E.CompE

toInt :: (Functor m, Monad m) => E -> CompE m (Maybe Integer)
