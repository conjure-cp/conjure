module Language.E.Pipeline.ConjureRepr where

import Language.E
import Language.E.Pipeline.ApplyRepr ( applyRepr )


conjureRepr :: (Monad m, Functor m)
    => Spec
    -> [RuleRepr]
    -> CompE m Spec
conjureRepr = applyRepr
