{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.E.Pipeline.ConjureAll
    ( conjureAllPure
    , conjureRandomPure
    , conjureWithMode
    ) where

import Conjure.Mode

import Language.E
import Language.E.Pipeline.ConjureRepr
import Language.E.Pipeline.ConjureRefn
import Language.E.Pipeline.Groom ( groomSpec )


conjureAllPure
    :: ConjureMode
    -> [RuleRepr] -> [RuleRefn] -> Spec
    -> [(Either Doc Spec, LogTree)]
conjureAllPure mode reprs refns spec = onlyOneError $ runCompE "conjure" $ do
    modifyGlobal $ \ gl -> gl { conjureMode = mode }
    conjureAll reprs refns spec

conjureRandomPure
    :: StdGen -> ConjureMode
    -> [RuleRepr] -> [RuleRefn] -> Spec
    -> [(Either Doc Spec, LogTree)]
conjureRandomPure seed mode reprs refns spec = onlyOneError $ runCompE "conjure" $ do
    set_stdgen seed
    modifyGlobal $ \ gl -> gl { conjureMode = mode }
    conjureAll reprs refns spec

conjureWithMode
    :: ConjureMode
    -> [RuleRepr] -> [RuleRefn] -> Spec
    -> [(Either Doc Spec, LogTree)]
conjureWithMode mode reprs refns spec = onlyOneError $ runCompE "conjure" $ do
    modifyGlobal $ \ gl -> gl { conjureMode = mode }
    conjureAll reprs refns spec


onlyOneError :: [(Either a b, c)] -> [(Either a b, c)]
onlyOneError [] = []
onlyOneError (x:xs)
    | isLeft (fst x) = [x]
    | otherwise      = x : onlyOneError xs

data Which = GeneratesNone | GeneratesSome

conjureAll
    :: forall m
    .  MonadConjureList m
    => [RuleRepr] -> [RuleRefn] -> Spec -> m Spec
conjureAll reprs refns = phaseRepr0
    where
        ifNone
            :: (Spec -> m Spec)
            -> (Spec -> m Spec)
            -> (Spec -> m Spec)
            -> Spec -> m Spec
        ifNone ma mIf mElse s = flip evalStateT GeneratesSome $ do
            s' <- catchError (lift $ ma s) $ \ e ->
                    case e of
                        (ErrGeneratesNone,_,_) -> do put GeneratesNone ; lift $ mIf s
                        _                      -> throwError e
            w <- get
            case w of
                GeneratesNone -> return s'
                GeneratesSome -> lift $ mElse s'

        -- conjure phases, these run only once
        repr, refn, groom :: Spec -> m Spec
        -- never returns [], might raise ErrGeneratesNone, that is when we groom and go home.
        repr  s = trace "repr"  $ conjureRepr reprs s
        -- never returns [], neither raises ErrGeneratesNone. always go to repr after this.
        refn  s = trace "refn"  $ conjureRefn reprs refns s
        -- prepare the spec for outputting.
        groom s = trace "groom" $ groomSpec s

        -- conjure phases, these call the appropriate phase following them.
        -- calling phaseRepr0 will run the whole of conjure.
        phaseRepr0, phaseRepr, phaseRefn :: Spec -> m Spec
        phaseRepr0 = ifNone repr (refn >=> groom) phaseRefn
        phaseRepr  = ifNone repr groom            phaseRefn
        phaseRefn  = refn >=> phaseRepr

