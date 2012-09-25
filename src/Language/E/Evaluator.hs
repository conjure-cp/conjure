{-# LANGUAGE OverloadedStrings #-}

module Language.E.Evaluator ( simplify, trySimplifySpec, test_Simplify ) where

import Stuff.Pretty
import Stuff.FunkyT
import Stuff.NamedLog
import Language.E.Imports
import Language.E.Definition
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
            -- print $ prettyAsTree x
            let (results, globalSt) = runIdentity $ runCompE $ runWriterT $ simplify x
            print $ prettyLogs $ logs globalSt
            forM_ results $ \ (result, _) -> do
                case result of
                    Left e -> error (show e)
                    Right (y, Any flag) ->
                        if flag
                            then do
                                print $ pretty y
                                -- print $ prettyAsTree y
                            else return ()


trySimplifySpec :: (Functor m, Monad m) => Spec -> CompE m Spec
trySimplifySpec (Spec v xs) = do
    (xs', Any _flag) <- runWriterT (mapM simplify xs)
    return $ Spec v xs'


simplify :: (Functor m, Monad m) => E -> WriterT Any (FunkyT LocalState GlobalState CompError m) E
-- simplify = return
simplify x = do
    -- lift $ mkLog "debug:simplify" $ pretty x
    rewriteM (\ i -> firstJust $ map ($ i) [ loggedFullEvaluator
                                           , loggedEvalHasRepr
                                           , loggedEvalHasType
                                           , loggedEvalHasDomain
                                           , loggedEvalDomSize
                                           , loggedEvalIndices
                                           , loggedEvalReplace
                                           , loggedPartialEvaluator
                                           ]
             ) x
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
