{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
module Language.E.Up.Representations(
     leafRep, getBranch
    , runBranchFuncs
    , LeafFunc, BranchFunc
    , Before, After
    , RepName, isBranchRep, noRep

    -- for debuging
    , setOccurrenceRep, matrix1DRep, partitionMSetOfSetsRep
    ) where

import Language.E hiding (trace)
import Language.E.Up.Data
import Language.E.Up.Common(wrapInMatrix,unwrapMatrix,unwrapMatrix')
import Language.E.Up.Debug

-- Types
type LeafFunc   = (VarData ->  E)
type BranchFunc = (VarData ->  VarData)
type Before     = (VarData -> [VarData])
type After      = (VarData -> [VarData] -> VarData)
type RepName    = String


leafRep ::  String -> LeafFunc
leafRep kind =
    case kind of
      "SetExplicit"                   -> explicitRep
      "Explicit"                      -> explicitRep
      "MSetExplicit"                  -> explicitRep
      "SetOccurrence"                 -> setOccurrenceRep
      "Occurrence"                    -> setOccurrenceRep
      "Matrix1D"                      -> matrix1DRep
      "RelationIntMatrix2"            -> relationIntMatrix2Rep
      "SetExplicitVarSizeWithDefault" -> setExplicitVarSizeWithDefaultRep
      _                               -> noRep


noRep :: VarData -> E
noRep  = vEssence

{- Sets -}

explicitRep :: VarData -> E
explicitRep VarData{vEssence=e} =  e
    `_t` "explicitRep"


setOccurrenceRep :: VarData -> E
setOccurrenceRep VarData{vIndexes=[ix],
  vEssence=[xMatch| vs :=  value.matrix.values |]} =
    wrapInMatrix .  map (toIntLit . fst) . onlySelectedValues . zip ix $
        tracee "setOccurrenceRep" vs

-- CHECK should not really need this
setOccurrenceRep v@VarData{vIndexes=ix,
  vEssence=[xMatch| vs :=  value.matrix.values |]} =
    wrapInMatrix $ map (\f -> setOccurrenceRep v{vIndexes=tail ix, vEssence=f} ) $
       tracee "setOccurrenceRep" vs

setOccurrenceRep v = error $  "setOccurrenceRep " ++  (show . pretty) v

setExplicitVarSizeWithDefaultRep :: VarData -> E
setExplicitVarSizeWithDefaultRep VarData{vEssence=e,vBounds=bs} =
    explicitVarSizeWithDefault e

    where

    explicitVarSizeWithDefault :: E -> E
    explicitVarSizeWithDefault f =
        wrapInMatrix . mapMaybe (removeIt $ toRemove bs) . unwrapMatrix $
        tracee "setExplicitVarSizeWithDefaultRep" f

    toRemove :: [Integer] -> Integer
    toRemove []  = _bugg "setExplicitVarSizeWithDefaultRep no bounds"
    toRemove bs'  = last bs'

    removeIt :: Integer -> E -> Maybe E
    removeIt toRm f@[xMatch| [Prim (I i)] := value.literal |] =
       if i == toRm then Nothing else Just f

    removeIt _ f@[xMatch| _ := value.matrix.values |] =
        Just $ explicitVarSizeWithDefault f


    removeIt i f = errpM ("removeIt " ++ show i) [f]

{- Relations -}

relationIntMatrix2Rep :: VarData -> E
relationIntMatrix2Rep VarData{vIndexes=[a,b],
                              vEssence=[xMatch| vs := value.matrix.values |] } =
  tracer "relationIntMatrix2d:" values
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


relationIntMatrix2Rep v@VarData{vIndexes = (_:rest@(_:_)),
                                vEssence = [xMatch| vs := value.matrix.values |] } =
    wrapInMatrix . map (\w ->  relationIntMatrix2Rep v{vIndexes=rest,vEssence=w} ) $ vs


relationIntMatrix2Rep v = _bug "relationIntMatrix2Rep" [v]


{- Functions -}

matrix1DRep :: VarData -> E
matrix1DRep v@VarData{vIndexes=[ix], vEssence=[xMatch| vs :=  value.matrix.values |]} =
    let mappings = zipWith makeMapping ix vs
    in  wrapInFunction  mappings
    `_p` ("matrix1DRep v",[v] )
    `_p` ("matrix1DRep res",[mappings] )

    where
    makeMapping :: Integer -> E -> E
    makeMapping i f =  [xMake| mapping := [toIntLit i, f] |]

matrix1DRep v@VarData{vIndexes=(_:ix), vEssence=[xMatch| vs :=  value.matrix.values |]} =
    wrapInMatrix . map (\w -> matrix1DRep v{vIndexes=ix,vEssence=w} ) $ vs


matrix1DRep  v = error $  "matrix1DRep! " ++  (show . pretty) v

{- Partitions -}

partitionMSetOfSetsRep :: VarData -> E
partitionMSetOfSetsRep VarData{vEssence=[xMatch| vs :=  value.matrix.values |]} =
    let parts = map toPart vs
    in  tracee "partitionMSetOfSetsRep" [xMake| value.partition.values := parts |]

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


getBranch :: String -> Maybe (Before,After)
getBranch s =
    case s of
      "Matrix1D"           -> Just matrix1DBranch
      "SetExplicit"        -> Just explicitBranch
      "Explicit"           -> Just explicitBranch
      "MSetExplicit"       -> Just explicitBranch
      "SetOccurrence"      -> Just occurrenceBranch
      "Occurrence"         -> Just occurrenceBranch
      "MSetOfSets"         -> Just partitionMSetOfSetsBranch
      "SetExplicitVarSize" -> Just setExplicitVarSizeBranch
      "RelationAsSet"      -> Just relationAsSetRep
      "AsReln"             -> Just functionAsRelnRep
      _                    -> Nothing


isBranchRep :: RepName -> Bool
isBranchRep "Matrix1D"           = True
isBranchRep "MSetOfSets"         = True
isBranchRep "SetExplicitVarSize" = True
isBranchRep "RelationAsSet"      = True
isBranchRep "AsReln"             = True
isBranchRep "SetExplicit"        = True
isBranchRep "Explicit"           = True
isBranchRep "MSetExplicit"       = True
isBranchRep "SetOccurrence"      = True
isBranchRep "Occurrence"         = True
isBranchRep _                    = False


{- Sets -}

explicitBranch :: (Before,After)
explicitBranch = ( tracee "explicitBranch" unwrapSet, mapLeafUnchanged )

occurrenceBranch :: (Before,After)
occurrenceBranch = (tracee "occurrenceBranch" unwrapSet, mapLeafFunc setOccurrenceRep )

setExplicitVarSizeBranch :: (Before,After)
setExplicitVarSizeBranch = ( tracee "setExplicitVarSizeBranch" unwrapSet, after )

    where
    after orgData vs =
        let res =  explicitVarSize (map vEssence vs)
        in orgData{vEssence=res}
        `_p` ("explicitVarSize", [res])

    explicitVarSize :: [E] -> E
    explicitVarSize vs =
        wrapInMatrix $ mapMaybe getInSet (tracer "afterVarSize vs" vs)

    getInSet ::E -> Maybe E
    getInSet [eMatch| (true,&v)  |] = Just v
    getInSet [eMatch| (false,&_) |] = Nothing
    getInSet [xMatch| vs := value.matrix.values |] = Just $ explicitVarSize vs

    getInSet e  = _bug "setExplicitVarSizeBranch: getInSet" [e]


{- Relations -}

relationAsSetRep :: (Before,After)
relationAsSetRep = ( tracee "relationAsSet" beforeUnchanged, after)

    where
    after orgData [VarData{vEssence=e}] =
        let res =  relationAsSet e
        in  orgData{vEssence=res}
        `_p` ("relationAsSet res",  [res])
        `_p` ("relationAsSet e", [e])

    relationAsSet :: E -> E
    relationAsSet [xMatch| vs := value.matrix.values
                         | _  := value.matrix.values.value.matrix.values |] =
        wrapInMatrix . map relationAsSet $ vs

    relationAsSet [xMatch| vs := value.matrix.values |] = wrapInRelation vs


{- Functions -}

matrix1DBranch :: (Before,After)
matrix1DBranch = ( tracee "matrix1DBranch" unwrapSet, after )

    where
    after orgData@VarData{vIndexes=ix} vs =
        let wraped = wrapInMatrix $ map vEssence vs
        -- CHECK Seems a bit a hackish to only pass the first index
        in orgData{vEssence=matrix1DRep orgData{vIndexes=[head ix], vEssence=wraped}}



functionAsRelnRep :: (Before, After)
functionAsRelnRep = (  tracee "functionAsRelnRep" beforeUnchanged, after )

    where
    after orgData [v] = orgData{vEssence=functionAsReln . vEssence $ tracer "functionAsRelnRep v:" v }

    functionAsReln [xMatch| vs := value.matrix.values |] =
        wrapInMatrix . map functionAsReln $ vs

    functionAsReln f =  wrapInFunction . map relnToFunc .  unwrapRelation $ f

    relnToFunc [eMatch| (&from,&to) |] = [xMake| mapping := [from,to] |]


{- Partitions -}

partitionMSetOfSetsBranch :: (Before,After)
partitionMSetOfSetsBranch = ( tracee "partitionMSetOfSetsBranch" beforeUnchanged , after )
    where
    after orgData [vs] = orgData{vEssence=partitionMSetOfSetsRep vs}
        `_p` ("partitionMSetOfSetsBranch v", [vs])


{- End -}

beforeUnchanged :: VarData -> [VarData]
beforeUnchanged v = [v]

unwrapSet :: Before
unwrapSet v@VarData{vEssence=e, vIndexes=ix} =
        map (\f -> v{vEssence=f, vIndexes=tail ix} )  (unwrapMatrix' "unwrapSet" e)


mapLeafFunc :: LeafFunc -> VarData -> [VarData] -> VarData
mapLeafFunc f orgData vs =
        orgData{vEssence = wrapInMatrix . map f $  vs }

mapLeafUnchanged :: VarData -> [VarData] -> VarData
mapLeafUnchanged = mapLeafFunc vEssence

_afterErr ::Pretty a => a -> t
_afterErr = errp 

-- Utility functions

onlySelectedValues :: [(a,E)] -> [(a,E)]
onlySelectedValues = filter f
    where
    f (_,[eMatch| true|]) = True
    f (_,[eMatch| 1   |]) = userErr "1 should not be used as True"
    f (_,[eMatch| 0   |]) = userErr "0 should not be used as False"
    f (_,_) = False


wrapInFunction :: [E] -> E
wrapInFunction es = [xMake| value.function.values := es |]

wrapInRelation :: [E] -> E
wrapInRelation es = [xMake| value.relation.values := es |]

unwrapRelation :: E -> [E]
unwrapRelation [xMatch| vs := value.relation.values |] = vs
unwrapRelation e = _bug "unwrapRelation" [e]

toIntLit j =  [xMake| value.literal := [Prim (I j)] |]
toIntLit :: Integer -> E


_bug  :: Pretty a => String -> [a] -> t
_bug  s = upBug  ("Representations: " ++ s)
_bugi :: (Show a,Pretty b) => String -> (a, [b]) -> t
_bugi s = upBugi ("Representations: " ++ s )
_bugg :: String -> t
_bugg s = _bug s ([] :: [E])

