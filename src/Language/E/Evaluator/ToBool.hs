{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Evaluator.ToBool where

import Stuff.Generic
import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import Language.E.Evaluator


toBool :: (Functor m, Monad m) => E -> CompE m (Maybe Bool)
toBool [xMatch| [Prim (B b)] := value.literal |] = return (Just b)
toBool p            = do
    (p', Any flag) <- runWriterT $ simplify p
    if flag
        then toBool p'
        else return Nothing
