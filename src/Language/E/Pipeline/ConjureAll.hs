{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.E.Pipeline.ConjureAll ( conjureAllPure ) where

import Language.E
import Language.E.Pipeline.ConjureRepr
import Language.E.Pipeline.ConjureRefn
import Language.E.Pipeline.Groom ( groomSpec )
import Language.E.Pipeline.InlineLettings


data Phase = Repr | Refn | Groom

conjureAllPure
    :: [RuleRepr] -> [RuleRefn] -> Spec
    -> [(Either Doc Spec, LogTree)]
conjureAllPure reprs refns = {-# SCC "conjureAllPure" #-} onlyOneError . go Repr
    where

        onlyOneError [] = []
        onlyOneError (x:xs)
            | isLeft (fst x) = [x]
            | otherwise      = x : onlyOneError xs

        go :: Phase -> Spec -> [(Either Doc Spec, LogTree)]
        go Repr s = {-# SCC "gRepr" #-} trace "Repr" $
            let
                mouts :: [(Either Doc Spec, LogTree)]
                mouts = runCompE "Repr" $ inlineLettings s >>= \ s' -> conjureRepr False s' reprs

                f :: (Either Doc Spec, LogTree) -> [(Either Doc Spec, LogTree)]
                f (Left  x, logs) = [(Left x, logs)]
                f (Right x, logs) = map (second (logTreeAppend logs)) (go Refn x)
            in
                if null mouts
                    then do
                        let mouts2 = trace "Refn2" $ runCompE "Refn2" $ inlineLettings s >>= \ s' -> conjureRefn False s' refns
                        let f2 (Left  x, logs) = [(Left x, logs)]
                            f2 (Right x, logs) = map (second (logTreeAppend logs)) (go Groom x)
                        concatMap f2 mouts2
                    else concatMap f mouts
        go Refn s = {-# SCC "gRefn" #-} trace "Refn" $
            let
                mouts :: [(Either Doc Spec, LogTree)]
                mouts = runCompE "Refn" $ conjureRefn False s refns

                f :: (Either Doc Spec, LogTree) -> [(Either Doc Spec, LogTree)]
                f (Left  x, logs) = [(Left x, logs)]
                f (Right x, logs) = map (second (logTreeAppend logs)) (go Repr x)
            in
                if null mouts
                    then go Groom s
                    else concatMap f mouts
        go Groom s = {-# SCC "gGroom" #-} trace "Groom" $
            let
                mout :: (Either Doc Spec, LogTree)
                mout = runCompESingle "Groom" $ groomSpec s
            in
                [mout]

