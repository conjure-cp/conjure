{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Evaluator.ToInt where

import Stuff.Generic
import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import Language.E.Evaluator


toInt :: (Functor m, Monad m) => E -> CompE m (Maybe Integer)
toInt [xMatch| [Prim (I i)] := value.literal |] = return (Just i)
toInt p            = do
    (p', Any flag) <- runWriterT $ simplify p
    if flag
        then toInt p'
        else return Nothing
