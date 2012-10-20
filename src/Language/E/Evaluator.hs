{-# LANGUAGE OverloadedStrings #-}

module Language.E.Evaluator ( simplify, trySimplifySpec, trySimplifyE, test_Simplify ) where

import Stuff.Pretty
import Stuff.FunkyT

import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import Language.E.Lexer
import Language.E.Parser
import Language.E.Evaluator.Full    ( fullEvaluator
                                    , evalHasType, evalHasDomain
                                    , evalHasRepr, evalDomSize
                                    , evalIndices, evalReplace
                                    )
import Language.E.Evaluator.Partial ( partialEvaluator )

import qualified Data.Text as T
import Data.Generics.Uniplate.Data ( rewriteM )


test_Simplify :: T.Text -> IO ()
test_Simplify t = do
    let res = (runLexer >=> runParser (inCompleteFile parseExpr) "") t
    case res of
        Left  e -> print e
        Right x -> do
            print $ pretty x
            ys <- runCompEIO $ runWriterT $ simplify x
            mapM_ (print . pretty . fst) ys


trySimplifySpec :: (Functor m, Monad m) => Spec -> CompE m Spec
trySimplifySpec (Spec v xs) = do
    let
        loggedSimplify i = do
            (i', Any b) <- listen (simplify i)
            when b $ lift $ mkLog "simplified" $ vcat [pretty i, "~~>", pretty i']
            return i'
    (xs', Any flag) <- runWriterT (mapM loggedSimplify xs)
    (if flag then trySimplifySpec else return) (Spec v xs')

trySimplifyE :: (Functor m, Monad m) => E -> CompE m E
trySimplifyE x = do
    (x', Any flag) <- runWriterT (simplify x)
    (if flag then trySimplifyE else return) x'

simplify :: (Functor m, Monad m) => E -> WriterT Any (FunkyT LocalState GlobalState (CompError, Maybe Spec) m) E
simplify =
    rewriteM (\ i -> firstJust $ map ($ i) [ loggedFullEvaluator
                                           , loggedEvalHasRepr
                                           , loggedEvalHasType
                                           , loggedEvalHasDomain
                                           , loggedEvalDomSize
                                           , loggedEvalIndices
                                           , loggedEvalReplace
                                           , loggedPartialEvaluator
                                           ]
             )
    where
        loggedFullEvaluator    = logged "Evaluator"                 fullEvaluator
        loggedEvalHasRepr      = logged "Evaluator.hasRepr"         evalHasRepr
        loggedEvalHasType      = logged "Evaluator.hasType"         evalHasType
        loggedEvalHasDomain    = logged "Evaluator.hasDomain"       evalHasDomain
        loggedEvalDomSize      = logged "Evaluator.domSize"         evalDomSize
        loggedEvalIndices      = logged "Evaluator.indices"         evalIndices
        loggedEvalReplace      = logged "Evaluator.replace"         evalReplace
        loggedPartialEvaluator = logged "Simplify"                  partialEvaluator

        logged str act inp = do
            moutp <- lift $ act inp
            case moutp of
                Nothing   -> return Nothing
                Just outp -> do
                    lift $ mkLog str $ pretty inp <+> "~~>" <+> pretty outp
                    tell (Any True)
                    return moutp

        firstJust []     = return Nothing
        firstJust (a:as) = do
            mr <- a
            case mr of
                Nothing -> firstJust as
                Just _  -> return mr
