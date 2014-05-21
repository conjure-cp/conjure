{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
module Language.E.Up.GatherIndexRanges(gatherIndexRanges) where

import Language.E
import Language.E.Up.Data
import Language.E.Up.Debug(upBug)
import Language.E.Up.ReduceSpec(specSimplify)

import Data.Map(Map)
import qualified Data.Map  as M
import qualified Data.Text as T


gatherIndexRanges :: Spec -> Map String IndexT 
gatherIndexRanges (Spec _ es) = M.fromList $ mapMaybe gatherIndexRange (statementAsList es)

gatherIndexRange :: E -> Maybe (String,IndexT)
gatherIndexRange [xMatch| [Prim (S name)] := topLevel.declaration.find.name.reference 
                        | [dom]           := topLevel.declaration.find.domain |] = 
    Just (T.unpack name,gatherIndexT dom)

gatherIndexRange _  = Nothing


gatherIndexT :: E -> IndexT

gatherIndexT [xMatch| [domRange] := domain.matrix.index 
                    | [dom]      := domain.matrix.inner |] =
   IndexMatrix sim (gatherIndexT dom) 

   where 
   sim :: E
   sim = let (Spec _ es) = specSimplify (Spec (LanguageVersion "Essence" [1,3]) domRange)
         in es

gatherIndexT [xMatch| doms := domain.tuple.inners |] =
   IndexTuple (map gatherIndexT doms)

gatherIndexT [xMatch| doms := domain.relation.inners |] =
   IndexRel (map gatherIndexT doms)

gatherIndexT [xMatch| [from] := domain.function.innerFrom
                    | [to]   := domain.function.innerTo|] =
   IndexFunc (gatherIndexT from) (gatherIndexT to)

gatherIndexT [xMatch| [dom] := domain.partition.inner |] =
   IndexPar (gatherIndexT dom)
 
gatherIndexT [xMatch| [dom] := domain.set.inner |] =
   IndexSet (gatherIndexT dom)

gatherIndexT [xMatch| [dom] := domain.mset.inner |] =
   IndexSet (gatherIndexT dom)

gatherIndexT (Tagged "reference" _) = IndexNone 

gatherIndexT (Tagged "domain" [Tagged tag _]) 
    | tag `elem` ["int", "bool", "enum" ] = IndexNone

gatherIndexT dom = upBug "gatherIndexRange: gatherIndexT domain not matched" [dom]

