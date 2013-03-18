{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings, FlexibleInstances #-}

module Language.E.GenerateRandomParam ( generateRandomParam ) where

import Language.E
import Language.E.DomainOf(domainOf)
{-import Language.E.Up.Debug(prettyAsBoth)-}
import Language.E.Up.IO(getSpec)
import Language.E.Up.ReduceSpec(reduceSpec)

type Essence     = Spec
type EsseceParam = Spec

generateRandomParam :: MonadConjure m => Essence -> m EsseceParam
generateRandomParam essence = do
    let stripped = stripDecVars essence
    reduced@(Spec _ e) <- reduceSpec stripped
    let es = statementAsList e

    mkLog "Givens" (pretty stripped)
    mkLog "Reduced" (pretty reduced)

    doms <-  mapM domainOf es 
    mkLog "Doms" (vcat $ map prettyAsPaths doms)
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


-- To use in ghci
instance Show LogTree where
   show = show . pretty 

_r :: Monad m => m Essence -> m [(Either Doc EsseceParam, LogTree)]
_r sp = do
  spec <- sp
  return $ runCompE "gen" $ generateRandomParam spec

_x :: [(Either Doc EsseceParam, LogTree)] -> IO () 
_x ((_, lg):_) =   print (pretty lg)
_x _ = return ()

_getTest :: FilePath -> IO Spec
_getTest f = getSpec $ "/Users/bilalh/CS/conjure/test/generateParams/" ++ f  ++ ".essence"

_e :: IO Spec
_e = _getTest "enum-1"
_f :: IO Spec
_f = _getTest "func-1"
_i :: IO Spec
_i = _getTest "int-1"
_l :: IO Spec
_l = _getTest "letting-1"
_p :: IO Spec
_p = _getTest "partition-1"
_s :: IO Spec
_s = _getTest "set-1"
