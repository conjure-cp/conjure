{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.E.Evaluator.ToBool where

import Stuff.Generic
import Stuff.NamedLog
import Stuff.Pretty
import Language.E.Imports
import Language.E.Definition
import Language.E.Evaluator


toBool :: (Functor m, Monad m) => E -> CompE m (Maybe Bool)
toBool (Prim (B b)) = return (Just b)
toBool [xMatch| [Prim (B b)] := value.literal |] = return (Just b)
toBool p            = do
    mkLog "debug:toBool" $ pretty p
    (p', Any flag) <- runWriterT $ simplify p
    if flag
        then toBool p'
        else return Nothing
