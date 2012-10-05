module Language.E.Pipeline.ConjureRepr where

import Language.E
import Language.E.Pipeline.ApplyRepr ( applyRepr )
import Language.E.Pipeline.Groom ( groomSpec )


conjureRepr :: (Monad m, Functor m)
    => Bool
    -> Spec
    -> [RuleRepr]
    -> CompE m Spec
conjureRepr isFinal spec rules =
    let pipeline = trySimplifySpec      -- to remove any unnecessary occurrences of variables
                >=> applyRepr rules
                >=> (if isFinal then groomSpec else return)
    in  pipeline spec
