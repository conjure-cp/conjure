module Language.E.Pipeline.ConjureRepr where

import Language.E
import Language.E.Pipeline.ApplyRepr ( applyRepr )
import Language.E.Pipeline.InlineLettings
-- import Language.E.Pipeline.RemoveUnused ( removeUnused )
import Language.E.Pipeline.Groom ( groomSpec )


conjureRepr
    :: ( MonadConjure m
       , MonadList m
       )
    => Bool
    -> Spec
    -> [RuleRepr]
    -> m Spec
conjureRepr isFinal spec rules = {-# SCC "conjureRepr" #-} withBindingScope' $ do
    let pipeline =  recordSpec >=> simplifySpec           -- to remove any unnecessary occurrences of variables
--                >=> recordSpec >=> removeUnused         -- and remove the declarations from the model too
                >=> recordSpec >=> applyRepr rules
                >=> recordSpec >=> (if isFinal then groomSpec else return)
                >=> recordSpec
    pipeline spec



conjureReprPure
    :: [RuleRepr] -> [RuleRefn] -> Spec
    -> [(Either Doc Spec, LogTree)]
conjureReprPure reprs _ = {-# SCC "conjureReprPure" #-} onlyOneError . go
    where

        onlyOneError [] = []
        onlyOneError (x:xs)
            | isLeft (fst x) = [x]
            | otherwise      = x : onlyOneError xs

        go :: Spec -> [(Either Doc Spec, LogTree)]
        go s = {-# SCC "gRepr" #-} trace "Repr" $
            let
                mouts :: [(Either Doc Spec, LogTree)]
                mouts = runCompE "Repr" $ inlineLettings s >>= \ s' -> conjureRepr False s' reprs

            in  mouts

