{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Language.E.Pipeline.ConjureAll ( conjureWithMode) where

import Conjure.Mode

import Language.E
import Language.E.Pipeline.ConjureRepr
import Language.E.Pipeline.ConjureRefn
import Language.E.Pipeline.Groom ( groomSpec )

import qualified Data.HashSet as S


conjureWithMode
    :: S.HashSet String
    -> StdGen
    -> Maybe Int
    -> ConjureModeWithFlags
    -> [RuleRepr] -> [RuleRefn] -> Spec
    -> [(Either Doc Spec, LogTree)]
conjureWithMode flags seed limit mode reprs refns spec = onlyOneError $ runCompE "conjure" $ do
    set_stdgen seed
    modifyGlobal $ \ gl -> gl { conjureMode  = mode
                              , conjureFlags = flags
                              }
    conjureAll limit reprs refns spec


onlyOneError :: [(Either a b, c)] -> [(Either a b, c)]
onlyOneError [] = []
onlyOneError (x:xs)
    | isLeft (fst x) = [x]
    | otherwise      = x : onlyOneError xs

data Which = GeneratesNone | GeneratesSome

conjureAll
    :: forall m
    .  MonadConjureList m
    => Maybe Int
    -> [RuleRepr] -> [RuleRefn] -> Spec -> m Spec
conjureAll limit reprs refns = phaseRepr0
    where
        ifNone
            :: (Spec -> m Spec)     -- run this
            -> (Spec -> m Spec)     -- run this with the result if no errors
            -> (Spec -> m Spec)     -- run this instead if ErrGeneratesNone
            -> Spec -> m Spec
        ifNone ma mIf mElse s = do
            (s',w) <- catchError (fmap (,GeneratesSome) (ma s)) $ \ e ->
                    case e of
                        (ErrGeneratesNone,_,_) -> fmap (,GeneratesNone) (mIf s)
                        _                      -> fmap (,GeneratesSome) (throwError e)
            case w of
                GeneratesNone -> return s'
                GeneratesSome -> mElse s'

        -- conjure phases, these run only once
        repr, refn, groom :: Spec -> m Spec
        -- never returns [], might raise ErrGeneratesNone, that is when we groom and go home.
        repr  s = trace "repr"  $ conjureRepr reprs s
        -- never returns [], neither raises ErrGeneratesNone. always go to repr after this.
        refn  s = trace "refn"  $ conjureRefn reprs refns s
        -- prepare the spec for outputting.
        groom s = trace "groom" $ groomSpec True s
        -- groom s = trace "groom" $ return s

        -- conjure phases, these call the appropriate phase following them.
        -- calling phaseRepr0 will run the whole of conjure.
        phaseRepr0 :: Spec -> m Spec
        phaseRepr0  = ifNone repr (refn >=> groom) (phaseRefn 0)

        phaseRepr :: Int -> Spec -> m Spec
        phaseRepr n s = do
            mkLog "repr" (pretty n)
            if limitReached n
                then do
                    mkLog "limit reached" (pretty n)
                    (refn >=> groom) s
                else ifNone repr groom (phaseRefn n) s

        phaseRefn :: Int -> Spec -> m Spec
        phaseRefn n s = do
            mkLog "refn" (pretty n)
            (refn >=> phaseRepr (n+1)) s

        limitReached n = case limit of
            Nothing -> False
            Just l  -> n >= l


