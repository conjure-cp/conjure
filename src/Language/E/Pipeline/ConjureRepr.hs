module Language.E.Pipeline.ConjureRepr where

import Language.E
import Language.E.Pipeline.ApplyRepr ( applyRepr )
import Language.E.Pipeline.InlineLettings ( inlineLettings )
import Language.E.Pipeline.IntroduceRegions ( introduceRegions )
import Language.E.Pipeline.HandlingEnums ( handleEnums )
import Language.E.Pipeline.NoTuples ( conjureNoTuples )


conjureRepr
    :: ( MonadConjure m
       , MonadList m
       )
    => [RuleRepr]
    -> Spec
    -> m Spec
conjureRepr reprs spec = withBindingScope' $ do
    initialiseSpecState spec
    let pipeline =  return
                >=> recordSpec >=> handleEnums
                >=> recordSpec >=> inlineLettings
                >=> recordSpec >=> simplifySpec           -- to remove any unnecessary occurrences of variables
                >=> recordSpec >=> conjureNoTuples
                >=> recordSpec >=> introduceRegions
                >=> recordSpec >=> applyRepr reprs
                >=> recordSpec
    pipeline spec



conjureReprPure
    :: [RuleRepr] -> [RuleRefn] -> Spec
    -> [(Either Doc Spec, LogTree)]
conjureReprPure reprs _ = onlyOneError . go
    where

        onlyOneError [] = []
        onlyOneError (x:xs)
            | isLeft (fst x) = [x]
            | otherwise      = x : onlyOneError xs

        go :: Spec -> [(Either Doc Spec, LogTree)]
        go s = trace "Repr" $
            let
                mouts :: [(Either Doc Spec, LogTree)]
                mouts = runCompE "Repr" $ conjureRepr reprs s

            in  mouts

