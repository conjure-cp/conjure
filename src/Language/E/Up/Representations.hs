{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
module Language.E.Up.Representations(
     leafRep, getBranch
    , runBranchFuncs
    , LeafFunc, BranchFunc
    , Before, After
    
    -- for debuging
    , setOccurrenceRep, matrix1DRep, partitionMSetOfSetsRep
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
      "SetExplicit"                  -> explicitRep
      "SetOccurrence"                -> setOccurrenceRep
      "Matrix1D"                     -> matrix1DRep
      "RelationIntMatrix2"           -> relationIntMatrix2Rep
      --"ExplicitVarSizeWithDefault" -> explicitVarSizeWithDefaultRep
      _                              -> noRep


noRep :: VarData -> E
noRep  = vEssence

{- Sets -}

explicitRep :: VarData -> E
explicitRep VarData{vEssence=e} = e


setOccurrenceRep :: VarData -> E
setOccurrenceRep VarData{vIndexes=[ix],
  vEssence=[xMatch| vs :=  value.matrix.values |]} =
    wrapInMatrix .  map (toIntLit . fst) . onlySelectedValues . zip ix $ vs

-- CHECK should not really need this
setOccurrenceRep v@VarData{vIndexes=ix,
  vEssence=[xMatch| vs :=  value.matrix.values |]} =
    wrapInMatrix $ map (\f -> setOccurrenceRep v{vIndexes=tail ix, vEssence=f} ) vs

setOccurrenceRep v = error $  "setOccurrenceRep " ++  (show . pretty) v

setExplicitVarSize  :: VarData -> E
setExplicitVarSize  vd = error "fdfdfd" -- errp vd


{- Relations -}

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


{- Functions -}

matrix1DRep :: VarData -> E
matrix1DRep VarData{vIndexes=(ix:_), vEssence=[xMatch| vs :=  value.matrix.values |]} =
    let mappings = zipWith makeMapping ix vs
        func     = [xMake| value.function.values := mappings |]
    in func

    where
    makeMapping :: Integer -> E -> E
    makeMapping i f =  [xMake| mapping := [toIntLit i, f] |]

matrix1DRep  v = error $  "matrix1DRep " ++  (show . pretty) v

{- Partitions -}

partitionMSetOfSetsRep :: VarData -> E
partitionMSetOfSetsRep VarData{vEssence=[xMatch| vs :=  value.matrix.values |]} =
    let parts = map toPart vs
    in  [xMake| value.partition.values := parts |]

    where
    toPart [xMatch| es := value.matrix.values |] =  [xMake| part := es |]

{- End -}


-- Branch funcs
runBranchFuncs :: [(Before, After)] -> VarData -> LeafFunc -> VarData
runBranchFuncs fs starting f = evalFs fs (liftRep f) starting

evalFs :: [(Before, After)] -> BranchFunc -> VarData -> VarData
evalFs []     mid v =  mid v
evalFs [g]    mid v =  evalF g mid v
evalFs (g:gs) mid v =  evalF g (evalFs gs mid ) v

-- Run the before transformations the inner function then the after transformation
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
      "Matrix1D"           -> [matrix1DBranch]
      "Explicit"           -> [explicitBranch]
      "Occurrence"         -> [occurrenceBranch]
      "MSetOfSets"         -> [partitionMSetOfSetsBranch]
      "SetExplicitVarSize" -> [setExplicitVarSizeBranch]
      _                    -> []

{- Sets -}

explicitBranch :: (Before,After)
explicitBranch = ( unwrapSet, mapLeafUnchanged )

occurrenceBranch :: (Before,After)
occurrenceBranch = ( unwrapSet, mapLeafFunc setOccurrenceRep )

setExplicitVarSizeBranch :: (Before,After)
setExplicitVarSizeBranch = ( unwrapSet, after )

    where 
    after orgData vs = errp vs 

{- Functions -}

matrix1DBranch :: (Before,After)
matrix1DBranch = ( unwrapSet, after )

    where
    after orgData vs =
        let wraped = wrapInMatrix $ map vEssence vs
        in orgData{vEssence=matrix1DRep orgData{vEssence=wraped}}


{- Partitions -}

partitionMSetOfSetsBranch :: (Before,After)
partitionMSetOfSetsBranch = ( before, after )
    where
    before v = [v]
    after orgData [vs] = vs{vEssence=partitionMSetOfSetsRep vs}


{- End -}

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
    f (_,[eMatch| 1   |]) = error "1 should not be used as True"
    f (_,[eMatch| 0   |]) = error "0 should not be used as False"
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

