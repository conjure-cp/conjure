{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
module Language.E.GenerateParam where

import Bug
import Language.E hiding (mkLog)
import Language.E.Imports
import Language.E.DomainOf(domainOf)
import Language.E.Up.Debug(upBug)
import Language.E.Up.IO(getSpec')
import Language.E.Up.ReduceSpec(reduceSpec,removeNegatives)
import Language.E.Up.GatherInfomation(getEnumMapping,getEnumsAndUnamed)
import Language.E.Up.EprimeToEssence(convertUnamed)

import Language.E.GenerateRandomParam.Common(mkLog)
import Language.E.GenerateRandomParam.Data

import Language.E.ValidateSolution(validateSolutionPure)

import System.Directory(getCurrentDirectory)
import System.FilePath((</>))
import Text.Groom(groom)

import Data.List(permutations,transpose,mapAccumL,foldl1')
import Control.Arrow((&&&),arr,(***),(|||),(+++))
import Language.E.NormaliseSolution(normaliseSolutionEs)

import qualified Data.Map as Map


generateParam :: (MonadConjure m, RandomM m) => Essence -> m EssenceParam
generateParam essence = do
    givens <- plumming essence

    result <- wrapping givens [[eMake| 1 |]] 
    mkLog "Result" (pretty result)
    return result

    where
    emptySolution = Spec ("Essence", [1,3]) (listAsStatement [])
    removeForValidate (Spec v es) = Spec v $ listAsStatement $  filter filterer es'
        where es' = statementAsList es
              filterer [xMatch| _ := topLevel.suchThat           |] = False
              filterer [xMatch| _ := topLevel.declaration.find   |] = False
              filterer _                              = True


plumming :: MonadConjure m => Spec -> m [E]
plumming essence' = do
    essence <- removeNegatives essence'
    let stripped@(Spec _ f) = stripDecVars essence
    (Spec _ e) <- reduceSpec stripped

    {-mkLog "GivensSpec"   (vcat .  map (pretty . prettyAsTree) $  (statementAsList f))-}
    mkLog "GivensSpec" (pretty f)

    let enumMapping1     = getEnumMapping essence
        enums1           = getEnumsAndUnamed essence
        (enumMapping, _) = convertUnamed enumMapping1 enums1
        filterer [xMatch| _ := topLevel.where  |] = False
        filterer _ = True
        es  = filter filterer (statementAsList e)

    mkLog "Reduced   " $ pretty es <+> "\n"
    mkLog "enums" (pretty . groom $ enumMapping)

    return es


wrapping :: (MonadConjure m, RandomM m) => [E] -> [E] -> m EssenceParam
wrapping givens vals = do
    let lettings = zipWith makeLetting givens vals 
    mkLog "Lettings" (vcat $ map pretty lettings)
    --mkLog "Lettings" (vcat $ map (\a -> prettyAsTree a <+> "\n" ) lettings )

    let essenceParam = Spec ("Essence", [1,3]) (listAsStatement lettings )
    return essenceParam


makeLetting :: E -> E -> E
makeLetting given val =
    [xMake| topLevel.letting.name := [getRef given]
          | topLevel.letting.expr := [val]|]

    where
    getRef :: E -> E
    getRef [xMatch|  _  := topLevel.declaration.given.name.reference
                  | [n] := topLevel.declaration.given.name |] = n
    getRef e = _bug "getRef: should not happen" [e]


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


{-
     Run easily from GHCI with
     _a  <name>
-}
_a :: FilePath -> IO ()
_a f = _x =<< _g  (_getTest  f)

_g :: IO Essence -> IO [(Either Doc EssenceParam, LogTree)]
_g sp = do
    seed <- getStdGen
    spec <- sp
    return $ runCompE "generateParam" (set_stdgen seed >> generateParam spec)

_x :: [(Either Doc a, LogTree)] -> IO ()
_x ((_, lg):_) =   print (pretty lg)
_x _ = return ()

_getTest :: FilePath -> IO Spec
_getTest f = do
    let dir = "/Users/bilalh/CS/paramgen/models/_other" 
    getSpec' False $ dir </>  f  </> f ++ ".essence"

_bug :: String -> [E] -> t
_bug  s = upBug  ("GenerateParam: " ++ s)
_bugg :: String -> t
_bugg s = _bug s []

