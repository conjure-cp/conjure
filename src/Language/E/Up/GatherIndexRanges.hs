{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
module Language.E.Up.GatherIndexRanges(gatherIndexRanges) where

import Bug
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

gatherIndexT (D (DomainBool{})) = IndexNone
gatherIndexT (D (DomainInt {}))= IndexNone
gatherIndexT (D (DomainEnum{})) = IndexNone
gatherIndexT (D (DomainTuple doms)) = IndexTuple (map (gatherIndexT . D) doms)
gatherIndexT (D (DomainSet _ dom)) = IndexSet (gatherIndexT (D dom))
gatherIndexT (D (DomainMSet _ dom)) = IndexSet (gatherIndexT (D dom))
gatherIndexT (D (DomainFunction _ from to)) = IndexFunc (gatherIndexT (D from)) (gatherIndexT (D to))
gatherIndexT (D (DomainRelation _ doms)) = IndexRel (map (gatherIndexT . D) doms)
gatherIndexT (D (DomainPartition _ dom)) = IndexPar (gatherIndexT (D dom))

gatherIndexT (D (DomainMatrix domRange dom)) = IndexMatrix sim (gatherIndexT (D dom))
    where 
        sim :: Domain
        sim = case specSimplify (Spec (LanguageVersion "Essence" [1,3]) (D domRange)) of
                Spec _ (D es) -> es
                s -> bug $ "gatherIndexT" <+> pretty s

gatherIndexT (Tagged "reference" _) = IndexNone 

gatherIndexT dom = upBug "gatherIndexRange: gatherIndexT domain not matched" [dom]

