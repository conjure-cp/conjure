{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Evaluator.ToInt where

import Stuff.Generic
-- import Stuff.Pretty
import Language.E.Imports
import Language.E.Definition
import Language.E.Evaluator


toInt :: (Functor m, Monad m) => E -> CompE m (Maybe Integer)
toInt (Prim (I i)) = return (Just i)
toInt [xMatch| [Prim (I i)] := value.literal |] = return (Just i)
toInt p            = do
    -- mkLog "debug:toInt" $ pretty p
    (p', Any flag) <- runWriterT $ simplify p
    if flag
        then toInt p'
        else return Nothing
