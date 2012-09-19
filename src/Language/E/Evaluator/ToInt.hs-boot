module Language.E.Evaluator.ToInt where

import Language.E.Definition

toInt :: (Functor m, Monad m) => E -> CompE m (Maybe Integer)
