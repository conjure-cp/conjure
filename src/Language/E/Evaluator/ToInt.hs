{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Evaluator.ToInt where

import Stuff.Generic
import Stuff.FunkyT
import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import Language.E.Evaluator


toInt :: (Functor m, Monad m) => E -> CompE m (Maybe (Integer, [Binder]))
toInt [xMatch| [Prim (I x)] := value.literal |] = return (Just (x, []))
toInt p            = do
    (p', (Any flag, bs)) <- runWriterT $ simplify p
    modifyLocal $ \ st -> st { binders = bs ++ binders st }
    if flag
        then do
            mres <- toInt p'
            case mres of
                Nothing         -> return Nothing
                Just (p'', bs2) -> return $ Just (p'', bs ++ bs2)
        else return Nothing

