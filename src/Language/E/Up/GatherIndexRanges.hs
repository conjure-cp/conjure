{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
module Language.E.Up.GatherIndexRanges(gatherIndexRanges) where

import Language.E
import Language.E.Up.Data
{-import Language.E.Up.Debug(upBug)-}

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
   IndexMatrix (wrapInIndexRange domRange) (gatherIndexT dom) 

gatherIndexT [xMatch| doms := domain.tuple.inners |] =
   IndexTuple (map gatherIndexT doms)

gatherIndexT [xMatch| [dom] := domain.set.inner |] =
   IndexSet (gatherIndexT dom)



gatherIndexT (Tagged "domain" [Tagged tag _]) | tag == "bool" || tag == "int" = IndexNone

gatherIndexT dom = error $ "\n" ++  (show . prettyAsTree) dom


wrapInIndexRange :: E -> E
wrapInIndexRange e = [xMake| indexrange := [e] |]


