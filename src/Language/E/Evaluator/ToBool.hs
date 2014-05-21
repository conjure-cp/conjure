{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Evaluator.ToBool where

import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import Language.E.Evaluator


-- returns Right (True|False, newBinders) if the expression can be reduced to a single bool.
-- returns Left x if it can't be. x is reduced as much as possible.
toBool :: MonadConjure m => E -> m (Either E (Bool, [Binder]))
toBool [xMatch| [Prim (B b)] := value.literal |] = return (Right (b, []))
toBool p            = do
    (p', (Any flag, bs)) <- runWriterT $ simplify p
    modify $ \ st -> st { binders = bs ++ binders st }
    if flag
        then do
            mres <- toBool p'
            case mres of
                Left p''         -> return $ Left p''
                Right (p'', bs2) -> return $ Right (p'', bs ++ bs2)
        else return $ Left p'

