{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.GenerateRandomParam ( generateRandomParam ) where

import Language.E


type Essence = Spec
type EsseceParam = Spec


generateRandomParam :: MonadConjure m => Essence -> m EsseceParam
generateRandomParam essence = do
    let stripped = stripDecVars essence
    return stripped


stripDecVars :: Essence -> Essence
stripDecVars (Spec v x) = Spec v y
    where
        xs = statementAsList x
        ys = filter stays xs
        y  = listAsStatement ys

        stays [xMatch| _ := topLevel.declaration.given |] = True
        stays [xMatch| _ := topLevel.letting           |] = True
        stays [xMatch| _ := topLevel.where             |] = True
        stays _ = False

