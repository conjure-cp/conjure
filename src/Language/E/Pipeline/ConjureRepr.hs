module Language.E.Pipeline.ConjureRepr where

import Language.E
import Language.E.Pipeline.ApplyRepr ( applyRepr )
-- import Language.E.Pipeline.RemoveUnused ( removeUnused )
import Language.E.Pipeline.Groom ( groomSpec )


conjureRepr :: (Monad m, Functor m)
    => Bool
    -> Spec
    -> [RuleRepr]
    -> CompE m Spec
conjureRepr isFinal spec rules = do
    initialiseSpecState spec
    let pipeline =  recordSpec >=> trySimplifySpec      -- to remove any unnecessary occurrences of variables
--                >=> recordSpec >=> removeUnused         -- and remove the declarations from the model too
                >=> recordSpec >=> applyRepr rules
                >=> recordSpec >=> (if isFinal then groomSpec else return)
                >=> recordSpec
    pipeline spec

