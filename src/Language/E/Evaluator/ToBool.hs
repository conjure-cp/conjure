{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Evaluator.ToBool where

import Stuff.Generic
import Stuff.FunkyT
import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import Language.E.Evaluator


toBool :: (Functor m, Monad m) => E -> CompE m (Maybe (Bool, [Binder]))
toBool [xMatch| [Prim (B b)] := value.literal |] = return (Just (b, []))
toBool p            = do
    (p', (Any flag, bs)) <- runWriterT $ simplify p
    modifyLocal $ \ st -> st { binders = bs ++ binders st }
    if flag
        then do
            mres <- toBool p'
            case mres of
                Nothing         -> return Nothing
                Just (p'', bs2) -> return $ Just (p'', bs ++ bs2)
        else return Nothing

