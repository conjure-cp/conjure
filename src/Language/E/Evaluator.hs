{-# LANGUAGE OverloadedStrings #-}

module Language.E.Evaluator ( simplify, trySimplifySpec, trySimplifyE ) where

import Stuff.Pretty

import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import Language.E.Helpers
import Language.E.Traversals
import Language.E.Lexer
import Language.E.Parser
import Language.E.Evaluator.Full    ( fullEvaluator
                                    , evalHasType, evalHasDomain
                                    , evalHasRepr, evalDomSize
                                    , evalIndices, evalReplace
                                    , tupleEq, matrixEq
                                    )
import Language.E.Evaluator.Partial ( partialEvaluator )

import qualified Data.Text as T


_testSimplify :: T.Text -> IO ()
_testSimplify t = do
    let res = (runLexer >=> runParser (inCompleteFile parseExpr) "") t
    case res of
        Left  e -> print e
        Right x -> do
            print $ pretty x
            ys <- runCompEIO $ runWriterT $ simplify x
            mapM_ (print . pretty . fst) ys


trySimplifySpec :: (Functor m, Monad m) => Spec -> CompE m Spec
trySimplifySpec (Spec v xs) = withBindingScope' $ do
    mapM_ introduceStuff xs
    let
        loggedSimplify i = do
            (i', (Any b, _)) <- listen (simplify i)
            when b $ lift $ mkLog "simplified" $ vcat [pretty i, "~~>", pretty i']
            return i'
    (xs', (Any flag, _)) <- runWriterT (mapM loggedSimplify xs)
    (if flag then trySimplifySpec else return) (Spec v xs')

trySimplifyE :: (Functor m, Monad m) => E -> CompE m (E, [Binder])
trySimplifyE x = do
    (x', (Any flag, bs)) <- runWriterT (simplify x)
    if flag
        then do
            (x'', bs2) <- trySimplifyE x'
            return (x'', bs ++ bs2)
        else
            return (x', bs)


simplify :: (Functor m, Monad m) => E -> WriterT (Any, [Binder]) (CompEMOnly m) E
simplify = traverseE allCombined
    where
        allCombined i =
            firstJustOr i
                $ map ($ i) [ loggedFullEvaluator
                            , loggedEvalHasRepr
                            , loggedEvalHasType
                            , loggedEvalHasDomain
                            , loggedEvalDomSize
                            , loggedEvalIndices
                            , loggedEvalReplace
                            , loggedPartialEvaluator
                            , logged "Evaluator.tupleEq"  tupleEq
                            , logged "Evaluator.matrixEq" matrixEq
                            ]

        loggedFullEvaluator    = logged "Evaluator"                 fullEvaluator
        loggedEvalHasRepr      = logged "Evaluator.hasRepr"         evalHasRepr
        loggedEvalHasType      = logged "Evaluator.hasType"         evalHasType
        loggedEvalHasDomain    = logged "Evaluator.hasDomain"       evalHasDomain
        loggedEvalDomSize      = logged "Evaluator.domSize"         evalDomSize
        loggedEvalIndices      = logged "Evaluator.indices"         evalIndices
        loggedEvalReplace      = logged "Evaluator.replace"         evalReplace
        loggedPartialEvaluator = logged "Simplify"                  (adapter partialEvaluator)

        adapter comp x = do
            y <- comp x
            case y of
                Nothing -> return Nothing
                Just z  -> return (Just (z, []))

        logged :: (Functor m, Monad m)
               => String
               -> (E -> CompE m (Maybe (E,[Binder])))
               -> E
               -> WriterT (Any, [Binder]) (CompEMOnly m) (Maybe E)
        logged str act inp = do
            moutp <- lift $ act inp
            case moutp of
                Nothing         -> return Nothing
                Just (outp, bs) -> do
                    lift $ mkLog str $ pretty inp <+> "~~>" <+> pretty outp
                    tell (Any True, bs)
                    return (Just outp)

        firstJustOr i []     = return i
        firstJustOr i (a:as) = do
            mr <- a
            case mr of
                Nothing -> firstJustOr i as
                Just r  -> return r

