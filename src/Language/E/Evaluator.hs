{-# LANGUAGE OverloadedStrings #-}

module Language.E.Evaluator ( simplify, trySimplifySpec ) where

import Stuff.Pretty
import Stuff.NamedLog
import Language.E.Imports
import Language.E.Definition
import Language.E.Lexer
import Language.E.Parser

import Language.E.Evaluator.Full    ( fullEvaluator, evalHasType, evalHasDomain )
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


trySimplifySpec (Spec v xs) = do
    (xs', Any flag) <- runWriterT (mapM simplify xs)
    return $ Spec v xs'


-- simplify :: (Functor m, Monad m) => E -> WriterT Any (CompE m) E
-- simplify = return
simplify x = do
    -- lift $ mkLog "debug:simplify" $ pretty x
    rewriteM (\ i -> firstJust [ logged "Evaluator"                     fullEvaluator    i
                               , logged "debug:Evaluator (has type)"    evalHasType      i
                               , logged "debug:Evaluator (has domain)"  evalHasDomain    i
                               , logged "Simplify "                     partialEvaluator i
                               ]
             ) x
    where
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
