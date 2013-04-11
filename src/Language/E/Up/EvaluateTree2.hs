{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}


module Language.E.Up.EvaluateTree2 (
     evalTree
    ,occurrenceRep, matrix1DRep,partitionMSetOfSetsRep
) where

import Language.E

import Language.E.Up.Data
import Language.E.Up.Common(transposeE,matrixToTuple,unwrapMatrix)
import Language.E.Up.Debug

import Data.Map(Map)
import qualified Data.Map as M
import Data.Set(Set)
import qualified Data.Set as S

type VarMap             = Map String VarData
type IsTuplesOfMatrixes = Set [String]

evalTree ::VarMap -> IsTuplesOfMatrixes -> Tree String  -> (String,E)
evalTree mapping set (Branch name arr) =
    (name, evalTree' mapping set [] [name] (repSelector arr))

evalTree mapping set tree@(Leaf name) =
    (name,  evalTree' mapping set [] []  tree)

evalTree _ _ _ = _bugg "evalTree no match"


evalTree' :: VarMap -> IsTuplesOfMatrixes -> [(Before,After)] -> [String] -> Tree String  -> E
evalTree' mapping set fs prefix (Leaf part) =
   let leafFunc = leafRep part
       res     = runBranchFuncs (reverse fs) vdata leafFunc
   in  vEssence res

     {-`_p` ("\n" ++ name, [vdata] )-}

    where
    name    = intercalate "_" (prefix ++ [part])
    lookUpE = fromMaybe (_bugg "fromMaybe: lookUpE evalTree'")  . flip M.lookup mapping
    vdata   = lookUpE  name

evalTree' mapping set fs prefix (Tuple arr) =
    let items =  map ( evalTree' mapping set fs prefix ) arr
        tuple = [xMake| value.tuple.values := items |]
        res   = handleTuplesOfMatrixes tuple
    in  res

     `_p` ("res",[res] )
     `_p` ("ans",[tuple] )
     `_g` ("prefix_tuple",prefix )


    where
    handleTuplesOfMatrixes :: E -> E
    handleTuplesOfMatrixes f | prefix `S.member` set = reverseTuplesOfMatrixes f
    handleTuplesOfMatrixes f = f


evalTree' mapping set fs prefix (Branch part@"ExplicitVarSize" arr) =
    error . show $  (pretty . groom) prefix  <+> (pretty . groom) mapping  <+>  (pretty . groom) arr

evalTree' mapping set fs prefix (Branch name arr) =
    evalTree' mapping set fs' (prefix ++ [name])  (repSelector arr)

    where
    fs' :: [(Before,After)]
    fs' = getBranch name ++ fs


-- Deals with (most) representations
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
  tuples (x,ys) = map (\y -> [xMake| value.tuple.values := (map wrap [x,y]) |]) ys
       where wrap i =  [xMake| value.literal := [Prim (I i)] |]

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


repSelector :: [Tree String] -> Tree String
repSelector arr = arr !! (length arr -1)


-- Branch funcs
type Before     = (VarData  -> [VarData])
type After      = (VarData -> [VarData] -> VarData)
type BranchFunc = (VarData  -> VarData)
type LeafFunc   = (VarData  -> E)

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
        mids   = tracer "mid:" $ map mid vs
        res    = tracer "after:" $ after value mids
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



reverseTuplesOfMatrixes ::  E -> E
reverseTuplesOfMatrixes [xMatch| vs := value.tuple.values |] =
    wrapInMatrix . map matrixToTuple $ transposeE vs

reverseTuplesOfMatrixes e = bug $ "reverseTuplesOfMatrixes called on " <+> pretty e


toIntLit :: Integer -> E
toIntLit j =  [xMake| value.literal := [Prim (I j)] |]


onlySelectedValues :: [(a,E)] -> [(a,E)]
onlySelectedValues = filter f
    where
    f (_,[eMatch| true|]) = True
    f (_,[eMatch| 1   |]) = True -- Temporary
    f (_,_) = False


wrapInMatrix :: [E] -> E
wrapInMatrix arr = [xMake| value.matrix.values := arr |]

wrapInRelation :: [E] -> E
wrapInRelation es = [xMake| value.relation.values := es |]


_bug :: String -> [E] -> t
_bug  s = upBug  ("EvaluateTree: " ++ s)
_bugi :: (Show a) => String -> (a, [E]) -> t
_bugi s = upBugi ("EvaluateTree: " ++ s )
_bugg :: String -> t
_bugg s = _bug s []

