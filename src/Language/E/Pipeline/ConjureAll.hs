{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.E.Pipeline.ConjureAll ( conjureAllPure ) where

import Language.E
import Language.E.Pipeline.ConjureRepr
import Language.E.Pipeline.ConjureRefn
import Language.E.Pipeline.Groom ( groomSpec )



conjureAllPure
    :: [RuleRepr] -> [RuleRefn] -> Spec
    -> [(Either Doc Spec, LogTree)]
conjureAllPure reprs refns spec = runCompE "conjure" $ conjureAll reprs refns spec


conjureAll
    :: MonadConjureList m
    => [RuleRepr] -> [RuleRefn] -> Spec -> m Spec
conjureAll reprs refns = phaseRepr0
    where
        ifNone ma mb = catchError ma $ \ e ->
            case e of
                (ErrGeneratesNone,_,_) -> mb
                _                      -> throwError e

        phaseRepr0 s =
            (conjureRepr reprs s >>= phaseRefn) `ifNone` phaseRefn s

        phaseRepr s =
            (conjureRepr reprs s >>= phaseRefn) `ifNone` phaseGroom s

        phaseRefn s =
            (conjureRefn refns s >>= phaseRepr) `ifNone` phaseGroom s

        phaseGroom s =
            groomSpec s

