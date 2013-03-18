{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings, FlexibleInstances #-}

module Language.E.GenerateRandomParam ( generateRandomParam ) where

import Language.E
import Language.E.DomainOf(domainOf)
--import Language.E.Up.Debug(prettyAsBoth)
import Language.E.Up.IO(getSpec)
import Language.E.Up.ReduceSpec(reduceSpec)

type Essence      = Spec
type EssenceParam = Spec


generateRandomParam :: MonadConjure m => Essence -> m EssenceParam
generateRandomParam essence = do
    let stripped@(Spec _ f) = stripDecVars essence
    reduced@(Spec v e) <- reduceSpec stripped
    let es = statementAsList e

    --mkLog "Spec" (vcat $ map (\a -> prettyAsPaths a <+> "\n" ) (statementAsList f) )
    mkLog "GivensSpec" (pretty stripped)
    mkLog "Reduced" (pretty reduced)

    doms <-  mapM domainOf es
    mkLog "Doms" (vcat $ map (\a -> prettyAsPaths a <+> "\n" ) doms )

    givens <-  mapM handleDomain doms
    mkLog "Givens" (vcat $ map pretty givens)

    let lettings = zipWith (makeLetting) es givens
    mkLog "Lettings" (vcat $ map pretty lettings)
    --mkLog "Lettings" (vcat $ map (\a -> prettyAsBoth a <+> "\n" ) lettings )

    let essenceParam = Spec v (listAsStatement lettings )
    --mkLog "EssenceParam" (pretty essenceParam)

    return essenceParam


makeLetting :: E -> E -> E
makeLetting given val =
    [xMake| topLevel.letting.name := [getRef given] 
          | topLevel.letting.expr.value := [val]|]

    where 
    getRef :: E -> E
    getRef [xMatch|  _  := topLevel.declaration.given.name.reference 
                  | [n] := topLevel.declaration.given.name |] = n

handleDomain :: MonadConjure m => E -> m E
handleDomain [xMatch| ranges := domain.int.ranges |] = do
    mkLog "int" (pretty ranges)
    vals <- mapM handleRange ranges
    return (head vals)

handleRange :: MonadConjure m => E -> m E
handleRange [xMatch| [Prim (I a),Prim (I b)] := range.fromTo.value.literal |] = do
   mkLog "fromTo" $ pretty (a,b)
   let val =  a + b `div` 2
   return $ Tagged "literal" [Prim (I val)]


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

_r :: Monad m => m Essence -> m [(Either Doc EssenceParam, LogTree)]
_r sp = do
  spec <- sp
  return $ runCompE "gen" $ generateRandomParam spec

_x :: [(Either Doc EssenceParam, LogTree)] -> IO ()
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
_i2 :: IO Spec
_i2 = _getTest "int-2"
_l :: IO Spec
_l = _getTest "letting-1"
_p :: IO Spec
_p = _getTest "partition-1"
_s :: IO Spec
_s = _getTest "set-1"

-- _x  =<<  _r _i
