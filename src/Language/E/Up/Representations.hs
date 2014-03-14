{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
module Language.E.Up.Representations(
      leafRep, noRep,
      getBranch, isBranchRep
    , runBranchFuncs
    , RepName, After, Before
    , representationsNames
    ) where

import Bug
import Language.E hiding (trace)
import Language.E.Up.Data
import Language.E.Up.Common(wrapInMatrix,unwrapMatrix,unwrapMatrix')
import Language.E.Up.Debug
import Data.List(genericReplicate)

import Data.List(genericTake)

representationsNames :: [String]
representationsNames = [
    "Function1D",
    "FunctionAsReln",
    "FunctionAsReln",
    "MSetExplicit",
    "MSetOccurrence",
    "RelationAsSet",
    "RelationIntMatrix2",
    "RelationIntMatrix3",
    "SetExplicit",
    "SetExplicitVarSize",
    "SetExplicitVarSizeWithDefault",
    "SetExplicitVarSizeWithMarker",
    "SetOccurrence",
    "AsReln",
    "Matrix1D",
    "PartitionSetOfSets",
    "SetGent",
    "FunctionIntPair2D"
    ]

-- Types
type LeafFunc   = (VarData ->  E)
type BranchFunc = (VarData ->  VarData)
type Before     = (VarData -> [VarData])
type After      = (VarData -> [VarData] -> VarData)
type RepName    = String


leafRep ::  String -> LeafFunc
leafRep kind =
    case kind of
      "MSetExplicit"                  -> explicitRep
      "MSetOccurrence"                -> msetOccurrenceRep
      "Matrix1D"                      -> matrix1DRep
      "Function1D"                    -> matrix1DRep
      "RelationIntMatrix2"            -> relationIntMatrix2Rep
      "RelationIntMatrix3"            -> relationIntMatrix3Rep
      "SetExplicit"                   -> explicitRep
      "SetExplicitVarSizeWithDefault" -> setExplicitVarSizeWithDefaultRep
      "SetOccurrence"                 -> setOccurrenceRep
      "SetGent"                       -> setGentRep
      "FunctionIntPair2D"             -> functionIntPair2DRep
      _                               -> noRep


noRep :: VarData -> E
noRep  = vEssence

{- Sets -}

explicitRep :: VarData -> E
explicitRep VarData{vEssence=e} =
    tracee "explicitRep" $ e

setGentRep :: VarData -> E
setGentRep VarData{vIndexes=[ix],
  vEssence=[xMatch| vs :=  value.matrix.values |]} =
    let
        gtZero [xMatch| [Prim (I x)] := value.literal |] = x > 0
        gtZero _ = False
    in
        wrapInMatrix .  map (toIntLit . fst) . filter (gtZero . snd) . zip ix $
            tracee "setGentRep" vs
setGentRep v = error $  "setGentRep " ++  (show . pretty) v


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


msetOccurrenceRep :: VarData -> E
msetOccurrenceRep VarData{vIndexes=[ix],
  vEssence=[xMatch| vs :=  value.matrix.values |]} =
    wrapInMatrix .  concatMap makeValues . selectedValues . zip ix $
        tracee "msetOccurrenceRep" vs

    where
        makeValues :: (Integer,E) -> [E]
        makeValues (value,count)  = map toIntLit $ genericReplicate (unwrapInt count) value

        selectedValues :: [(a,E)] -> [(a,E)]
        selectedValues                = filter f
        f (_,[eMatch| true|])         = userErr "True used for msetOccurrence"
        f (_,il) | unwrapInt il > 0   = True
        f (_,_)                       = False

-- CHECK should not really need this
msetOccurrenceRep v@VarData{vIndexes=ix,
  vEssence=[xMatch| vs :=  value.matrix.values |]} =
    wrapInMatrix $ map (\f -> msetOccurrenceRep v{vIndexes=tail ix, vEssence=f} ) $
       tracee "msetOccurrenceRep" vs

msetOccurrenceRep v = error $  "msetOccurrenceRep " ++  (show . pretty) v


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


relationIntMatrix3Rep :: VarData -> E
relationIntMatrix3Rep VarData{vIndexes=[a,b,c],
                              vEssence=[xMatch| vs := value.matrix.values |] } =
  tracer "relationIntMatrix3d:" values
  where
  values =
      wrapInRelation
    . map tuples
    . map flatten

    . concatMap expand
    . filter notEmpty
    . zip a 

    . map 
     ( concatMap expand .  filter notEmpty .  zip b 

     . map (  map fst . filter f . zip c . unwrapMatrix ) 
     . unwrapMatrix)
     $ vs


  tuples arr = [xMake| value.tuple.values := (map toIntLit arr) |]

  flatten :: (a,(a,a)) -> [a] 
  flatten (x,(y,z)) = [x,y,z]

  expand :: (a,[b]) -> [(a,b)]
  expand (x,ys) =  [ (x,y) |  y <- ys  ]

  notEmpty (_,[]) = False
  notEmpty _      = True

  f (_,[eMatch| true |])  = True
  f (_,[eMatch| false |]) = False
  f v = _bug "relationIntMatrix3Rep not boolean" [v]


relationIntMatrix3Rep v@VarData{vIndexes = (_:rest@(_:_)),
                                vEssence = [xMatch| vs := value.matrix.values |] } =
    wrapInMatrix . map (\w ->  relationIntMatrix3Rep v{vIndexes=rest,vEssence=w} ) $ vs

relationIntMatrix3Rep v = _bug "relationIntMatrix3Rep" [v] 


{- Functions -}


functionIntPair2DRep :: VarData -> E 
functionIntPair2DRep VarData{vIndexes=[a,b],
                              vEssence=[xMatch| vs := value.matrix.values |] } =  
  tracer "functionIntPair2DRep" values
  where
  values =
       wrapInFunction
     . map f
     . concatMap expand
     . zip a
     . map (  zip b . unwrapMatrix  )
     $ vs

  f :: (Integer, (Integer, E) ) -> E
  f (i, (j, e) ) = [xMake| mapping := [ t,  e] |]
    where t = [xMake| value.tuple.values := map toIntLit [i, j] |]

  expand :: (a,[b]) -> [(a,b)]
  expand (x,ys) =  [ (x,y) |  y <- ys  ]


matrix1DRep :: VarData -> E
matrix1DRep v@VarData{vIndexes=[ix], vEssence=[xMatch| vs :=  value.matrix.values |]} =
    let mappings = zipWith makeMapping ix vs
    in  wrapInFunction  mappings
    `_p` ("matrix1DRep res",[mappings] )
    `_p` ("matrix1DRep v",[v] )

    where
    makeMapping :: Integer -> E -> E
    makeMapping i f =  [xMake| mapping := [toIntLit i, f] |]

matrix1DRep v@VarData{vIndexes=(_:ix), vEssence=[xMatch| vs :=  value.matrix.values |]} =
    wrapInMatrix . map (\w -> matrix1DRep v{vIndexes=ix,vEssence=w} ) $ vs


matrix1DRep  v = error $  "matrix1DRep! " ++  (show . pretty) v

{- Partitions -}

partitionSetOfSetsRep :: VarData -> E
partitionSetOfSetsRep VarData{vEssence=[xMatch| vs :=  value.matrix.values |]} =
    let parts = map toPart vs
    in  tracee "partitionSetOfSetsRep" [xMake| value.partition.values := parts |]

    where
    toPart [xMatch| es := value.matrix.values |] =  [xMake| part := es |]
    toPart f = _bug "partitionSetOfSetsRep: toPart unhandled" [f]

partitionSetOfSetsRep v =  _bug "partitionSetOfSetsRep unhandled" [v]

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
      "AsReln"             -> Just functionAsRelnRep
      "FunctionAsReln"     -> Just functionAsRelnRep
      "MSetExplicit"       -> Just explicitBranch
      "MSetOccurrence"     -> Just moccurrenceBranch
      "PartitionSetOfSets" -> Just partitionSetOfSetsBranch
      "Matrix1D"           -> Just matrix1DBranch
      "Function1D"         -> Just matrix1DBranch
      "RelationAsSet"      -> Just relationAsSetRep
      "SetExplicit"        -> Just explicitBranch
      "SetExplicitVarSize" -> Just setExplicitVarSizeBranch
      "SetExplicitVarSizeWithMarker" -> Just setExplicitVarSizeWithMarkerBranch
      "SetOccurrence"      -> Just occurrenceBranch
      "unwrapBranch£"      -> Just unwrapBranch
      _                    -> Nothing


isBranchRep :: RepName -> Bool
isBranchRep "AsReln"             = True
isBranchRep "FunctionAsReln"     = True
isBranchRep "MSetExplicit"       = True
isBranchRep "MSetOccurrence"     = True
isBranchRep "PartitionSetOfSets" = True
isBranchRep "Matrix1D"           = True
isBranchRep "Function1D"         = True
isBranchRep "RelationAsSet"      = True
isBranchRep "SetExplicit"        = True
isBranchRep "SetExplicitVarSize" = True
isBranchRep "SetExplicitVarSizeWithMarker" = True
isBranchRep "SetOccurrence"      = True
isBranchRep "unwrapBranch£"      = True
isBranchRep _                    = False


{- Sets -}

explicitBranch :: (Before,After)
explicitBranch = ( tracee "explicitBranch" unwrapSet, mapLeafUnchanged )

-- TODO these two could be shorter
occurrenceBranch :: (Before,After)
occurrenceBranch = (tracee "occurrenceBranch" unwrapSet, mapLeafFunc setOccurrenceRep )

moccurrenceBranch :: (Before,After)
moccurrenceBranch = (tracee "moccurrenceBranch" unwrapSet, mapLeafFunc msetOccurrenceRep )

setExplicitVarSizeBranch :: (Before,After)
setExplicitVarSizeBranch = ( tracee "setExplicitVarSizeBranch" unwrapSet, after )

    where
    _before v = trace ("setExplicitVarSizeVV\n" ++ (show . pretty)  v) $ unwrapSet v

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


setExplicitVarSizeWithMarkerBranch :: (Before,After)
setExplicitVarSizeWithMarkerBranch = ( tracee "setExplicitVarSizeWithMarkerBranch" before, after )

    where
    before :: Before
    {-before v@VarData{vEssence=[eMatch| (&marker,&mat) |]} =  [v]-}
    before v =
        {-trace ("setExplicitVarSizeWithMarkerBranchr\n" ++ (show . pretty)  v) $-}
        [v]

    after orgData [VarData{vEssence=e}] = orgData{vEssence=explicitVarSizeWithMarker e }
    after _ vvs = _bug "setExplicitVarSizeWithMarkerBranch after unhandled" vvs

    explicitVarSizeWithMarker :: E -> E
    explicitVarSizeWithMarker [eMatch| (&marker,&mat) |] =
        wrapInMatrix $ genericTake (unwrapInt marker) (unwrapMatrix mat)

    explicitVarSizeWithMarker [xMatch| vs := value.matrix.values |] = -- trace (show . pretty $ vs) $
        wrapInMatrix $  map explicitVarSizeWithMarker vs

    explicitVarSizeWithMarker f = _bug "setExplicitVarSizeWithMarkerBranch unhandled" [f]


{- Relations -}

relationAsSetRep :: (Before,After)
relationAsSetRep = ( tracee "relationAsSet" beforeUnchanged, after)

    where
    after orgData [VarData{vEssence=e}] =
        let res =  relationAsSet e
        in  orgData{vEssence=res}
        `_p` ("relationAsSet res",  [res])
        `_p` ("relationAsSet e", [e])

    after _ vvs = _bug "relationAsSetRep unhandled" vvs

    relationAsSet :: E -> E
    relationAsSet [xMatch| vs := value.matrix.values
                         | _  := value.matrix.values.value.matrix.values |] =
        wrapInMatrix . map relationAsSet $ vs

    relationAsSet [xMatch| vs := value.matrix.values |] = wrapInRelation vs

    relationAsSet f = _bug "relationAsSetRep: relationAsSet unhandled" [f]


{- Functions -}

matrix1DBranch :: (Before,After)
matrix1DBranch = ( tracee "matrix1DBranch" unwrapSet, after )

    where
    after orgData@VarData{vIndexes=ix} vs =
        let wraped = wrapInMatrix $ map vEssence
                {-$ (trace . show . pretty $ orgData)-}
                {-trace "----------" $-}
                {-(trace . show . pretty $ vs)-}
                vs
        -- FIXME hackish to only pass the first index and breaks some tests
        in orgData{vEssence=matrix1DRep orgData{vIndexes=[head ix], vEssence=wraped}}



functionAsRelnRep :: (Before, After)
functionAsRelnRep = (  tracee "functionAsRelnRep" beforeUnchanged, after )

    where
    after orgData [v] = orgData{vEssence=functionAsReln . vEssence $ tracer "functionAsRelnRep v:" v }

    after _ vvs = _bug "functionAsRelnRep unhandled" vvs


    functionAsReln [xMatch| vs := value.matrix.values |] =
        wrapInMatrix . map functionAsReln $ vs

    functionAsReln f =  wrapInFunction . map relnToFunc .  unwrapRelation $ f

    relnToFunc [eMatch| (&from,&to) |] = [xMake| mapping := [from,to] |]

    relnToFunc f =  _bug "functionAsRelnRep: relnToFunc unhandled" [f]

{- Partitions -}

partitionSetOfSetsBranch :: (Before,After)
partitionSetOfSetsBranch = ( tracee "partitionSetOfSetsBranch" beforeUnchanged , after )
    where
    after orgData [vs] = orgData{vEssence=partitionSetOfSetsRep vs}
        `_p` ("partitionSetOfSetsBranch v", [vs])

    after _ vvs = _bug "partitionSetOfSetsBranch unhandled" vvs


{- End -}

-- FIXME  reps should not try to refine marker, part 
unwrapBranch :: (Before,After)
unwrapBranch = (tracee "unwrapBranch" unwrapSetMaybe, after)
    where
    after v@VarData{vEssence=[xMatch| _ := value.literal |]} _ =  v
    after orgData vs = orgData{vEssence=  wrapInMatrix $ map vEssence vs }

    unwrapSetMaybe :: Before
    unwrapSetMaybe v@VarData{vEssence=[xMatch| _ := value.literal |]} = [v] 
    unwrapSetMaybe v@VarData{vEssence=e, vIndexes=ix} =
        map (\f -> v{vEssence=f, vIndexes=tail ix} )  (unwrapMatrix' "unwrapSet" e) 


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

toIntLit :: Integer -> E
toIntLit j =  [xMake| value.literal := [Prim (I j)] |]

unwrapInt :: E -> Integer
unwrapInt [xMatch| [Prim (I j)] := value.literal  |] = j
unwrapInt  f = _bug "Not a int literal" [f]


_bug  :: Pretty a => String -> [a] -> t
_bug  s = upBug  ("Representations: " ++ s)
_bugi :: (Show a,Pretty b) => String -> (a, [b]) -> t
_bugi s = upBugi ("Representations: " ++ s )
_bugg :: String -> t
_bugg s = _bug s ([] :: [E])

