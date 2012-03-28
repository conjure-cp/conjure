module Language.Essence.Phases.CleanUp where

import Language.Essence.Spec
import Language.Essence.Expr
import Language.Essence.Op

cleanUp :: Spec -> Spec
cleanUp spec = spec { constraints = concatMap conjunct' $ constraints spec }

conjunct' :: Expr -> [Expr]
conjunct' (EOp And [x,y]) = conjunct' x ++ conjunct' y
conjunct' x = [x]
