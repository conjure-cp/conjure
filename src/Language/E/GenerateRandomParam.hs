{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings, FlexibleInstances #-}

module Language.E.GenerateRandomParam ( generateRandomParam ) where

import Language.E
import Language.E.Up.IO(getSpec)
import Language.E.DomainOf(domainOf)

type Essence     = Spec
type EsseceParam = Spec

generateRandomParam :: MonadConjure m => Essence -> m EsseceParam
generateRandomParam essence = do
    let stripped@(Spec v e) = stripDecVars essence
        es = statementAsList e
    doms <-  mapM domainOf es 

    mkLog "Givens" (pretty stripped)
    mkLog "Doms" (pretty doms)
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



instance Show LogTree where
   show = show . pretty 

_r :: Monad m => m Essence -> m [(Either Doc EsseceParam, LogTree)]
_r sp = do
  spec <- sp
  return $ runCompE "gen" $ generateRandomParam spec

_p :: [(Either Doc EsseceParam, LogTree)] -> IO () 
_p ((_, lg):_) =   print (pretty lg)
_p _ = return ()

_getTest :: FilePath -> IO Spec
_getTest f = getSpec $ "/Users/bilalh/CS/conjure/test/generateParams/" ++ f  ++ ".essence"

_1 :: IO Spec
_1 = _getTest "01-int"
