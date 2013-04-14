{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
module Language.E.Up.Representations(
     leafRep, getBranch
    , runBranchFuncs
    , LeafFunc, BranchFunc
    , Before, After
    
    -- for debuging
    , occurrenceRep, matrix1DRep, partitionMSetOfSetsRep
    ) where

import Language.E
import Language.E.Up.Data
import Language.E.Up.Common(wrapInMatrix,unwrapMatrix)
import Language.E.Up.Debug

-- Types 
type LeafFunc   = (VarData ->  E)
type BranchFunc = (VarData ->  VarData)
type Before     = (VarData -> [VarData])
type After      = (VarData -> [VarData] -> VarData)

leafRep ::  String -> LeafFunc
leafRep kind =
    case kind of
      "Explicit"   -> explicitRep
      "Occurrence" -> occurrenceRep
      "Matrix1D"   -> matrix1DRep
      "RelationIntMatrix2" -> relationIntMatrix2Rep
      --"ExplicitVarSizeWithDefault" -> explicitVarSizeWithDefaultRep
      _            -> noRep


noRep :: VarData -> E
noRep  = vEssence


explicitRep :: VarData -> E
explicitRep VarData{vEssence=e} = e


-- CHECK with Mset
occurrenceRep :: VarData -> E
occurrenceRep VarData{vIndexes=[ix],
  vEssence=[xMatch| vs :=  value.matrix.values |]} =
    wrapInMatrix .  map (toIntLit . fst) . onlySelectedValues . zip ix $ vs

-- FIXME should not really need this
occurrenceRep v@VarData{vIndexes=ix,
  vEssence=[xMatch| vs :=  value.matrix.values |]} =
    wrapInMatrix $ map (\f -> occurrenceRep v{vIndexes=tail ix, vEssence=f} ) vs

occurrenceRep v = error $  "occurrenceRep " ++  (show . pretty) v


relationIntMatrix2Rep :: VarData -> E
relationIntMatrix2Rep VarData{vIndexes=[a,b],
                              vEssence=[xMatch| vs := value.matrix.values |] } =
  values
  where
  values =
       wrapInRelation
     . concatMap tuples
     . filter notEmpty
     . zip a
     . map (map fst . filter f . zip b . unwrapMatrix)
     $ vs

  tuples :: (Integer,[Integer]) -> [E]
  tuples (x,ys) = map (\y -> [xMake| value.tuple.values := (map toIntLit [x,y]) |]) ys

  notEmpty (_,[]) = False
  notEmpty _      = True

  f (_,[eMatch| true |])  = True
  f (_,[eMatch| false |]) = False
  f _ = _bugg "relationIntMatrix2Rep not boolean"


matrix1DRep :: VarData -> E
matrix1DRep VarData{vIndexes=(ix:_), vEssence=[xMatch| vs :=  value.matrix.values |]} =
    let mappings = zipWith makeMapping ix vs
        func     = [xMake| value.function.values := mappings |]
    in func

    where
    makeMapping :: Integer -> E -> E
    makeMapping i f =  [xMake| mapping := [toIntLit i, f] |]

matrix1DRep  v = error $  "matrix1DRep " ++  (show . pretty) v


partitionMSetOfSetsRep :: VarData -> E
partitionMSetOfSetsRep VarData{vEssence=[xMatch| vs :=  value.matrix.values |]} =
    let parts = map toPart vs
    in  [xMake| value.partition.values := parts |]

    where
    toPart [xMatch| es := value.matrix.values |] =  [xMake| part := es |]



-- Branch funcs
runBranchFuncs :: [(Before, After)] -> VarData -> LeafFunc -> VarData
runBranchFuncs fs starting f = evalFs fs (liftRep f) starting

evalFs :: [(Before, After)] -> BranchFunc -> VarData -> VarData
evalFs []     mid v =  mid v
evalFs [g]    mid v =  evalF g mid v
evalFs (g:gs) mid v =  evalF g (evalFs gs mid ) v

-- Run the before transformtion the inner function then the after transformtion
evalF :: (Before,After) -> BranchFunc -> VarData -> VarData
evalF (before,after) mid value =
    let vs     = tracer "before:" $ before  (tracer "value:" value)
        mids   = tracer "mid:"    $ map mid vs
        res    = tracer "after:"  $ after value mids
    in res

-- Lift a LeafFunc to a Branch Func
liftRep ::  LeafFunc -> BranchFunc
liftRep repFunc vdata  = vdata{vEssence=repFunc vdata}



getBranch :: String -> [(Before,After)]
getBranch s =
    case tracer "\nBranchFunc " s of
      "Matrix1D"   -> [matrix1DBranch]
      "Explicit"   -> [explicitBranch]
      "Occurrence" -> [occurrenceBranch]
      "MSetOfSets" -> [partitionMSetOfSetsBranch]
      _            -> []


explicitBranch :: (Before,After)
explicitBranch = ( unwrapSet, mapLeafUnchanged )

occurrenceBranch :: (Before,After)
occurrenceBranch = ( unwrapSet, mapLeafFunc occurrenceRep )

matrix1DBranch :: (Before,After)
matrix1DBranch = ( unwrapSet, after )

    where
    after :: After
    after orgData vs =
        let wraped = wrapInMatrix $ map vEssence vs
        in orgData{vEssence=matrix1DRep orgData{vEssence=wraped}}


partitionMSetOfSetsBranch :: (Before,After)
partitionMSetOfSetsBranch = ( before, after )
    where
    before v = [v]
    after orgData [vs] = vs{vEssence=partitionMSetOfSetsRep vs}


unwrapSet :: Before
unwrapSet v@VarData{vEssence=e, vIndexes=ix} =
        map (\f -> v{vEssence=f, vIndexes=tail ix} )  (unwrapMatrix e)

mapLeafFunc :: LeafFunc -> VarData -> [VarData] -> VarData
mapLeafFunc f orgData vs =
        orgData{vEssence = wrapInMatrix . map f $  vs }

mapLeafUnchanged :: VarData -> [VarData] -> VarData
mapLeafUnchanged = mapLeafFunc vEssence


-- Utility functions

onlySelectedValues :: [(a,E)] -> [(a,E)]
onlySelectedValues = filter f
    where
    f (_,[eMatch| true|]) = True
    f (_,[eMatch| 1   |]) = True -- Temporary
    f (_,_) = False


wrapInRelation :: [E] -> E
wrapInRelation es = [xMake| value.relation.values := es |]

toIntLit :: Integer -> E
toIntLit j =  [xMake| value.literal := [Prim (I j)] |]


_bug :: String -> [E] -> t
_bug  s = upBug  ("Representations: " ++ s)
_bugi :: (Show a) => String -> (a, [E]) -> t
_bugi s = upBugi ("Representations: " ++ s )
_bugg :: String -> t
_bugg s = _bug s []

