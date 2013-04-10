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
    (name, evalTree' mapping set [name] (repSelector arr))

evalTree mapping set tree@(Leaf name) =
    (name,  evalTree' mapping set []  tree)

evalTree _ _ _ = _bugg "evalTree no match"


evalTree' :: VarMap -> IsTuplesOfMatrixes -> [String] -> Tree String  -> E
evalTree' mapping set prefix (Leaf part) =
   leafRepConverter part vdata
    {-in  error . show $ (res,name, vdata)-}
     `_p` ("\n" ++ name, [vdata] )

    where
    name    = intercalate "_" (prefix ++ [part])
    lookUpE = fromMaybe (_bugg "fromMaybe: lookUpE evalTree'")  . flip M.lookup mapping
    vdata   = lookUpE  name

evalTree' mapping set prefix (Tuple arr) =
    let items =  map ( evalTree' mapping set prefix ) arr
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


evalTree' mapping set prefix (Branch part@"Explicit" arr) =
    evalTree' mapping set (prefix ++ [part])  (repSelector arr)
    {-error . show $  (pretty . groom) prefix  <+> (pretty . groom) mapping  <+>  (pretty . groom) arr-}

evalTree' mapping set prefix (Branch part arr) =
    evalTree' mapping set (prefix ++ [part])  (repSelector arr)




-- Deals with (most) representations
leafRepConverter ::  String -> VarData -> E
leafRepConverter  kind  vdata@VarData{vEssence = es} =
    case kind of
      "Explicit"   -> explicitRep vdata
      "Occurrence" -> occurrenceRep vdata
      "Matrix1D"   -> matrix1DRep vdata 
      --"RelationIntMatrix2" -> relationIntMatrix2Rep vdata
      --"ExplicitVarSizeWithDefault" -> explicitVarSizeWithDefaultRep vdata
      _            -> es


explicitRep :: VarData -> E
explicitRep VarData{vEssence=e} = e 


-- CHECK with Mset
occurrenceRep :: VarData -> E
occurrenceRep VarData{vIndexes=[ix], 
  vEssence=[xMatch| vs :=  value.matrix.values |]} = 
    wrapInMatrix .  map (toIntLit . fst) . onlySelectedValues . zip ix $ vs

occurrenceRep v = error $  "occurrenceRep " ++  (show . pretty) v


matrix1DRep :: VarData -> E
matrix1DRep VarData{vIndexes=[ix], vEssence=[xMatch| vs :=  value.matrix.values |]} = 
    let mappings = zipWith makeMapping ix vs
        func     = [xMake| value.function.values := mappings |]
    in func

    where
    makeMapping :: Integer -> E -> E
    makeMapping i f =  [xMake| mapping := [toIntLit i, f] |]

matrix1DRep vd@VarData{vIndexes=ixs, vEssence=[xMatch| vs :=  value.matrix.values |]} =
    let res =zipWith (\e ix ->  matrix1DRep vd{vEssence=e,vIndexes=[ix]} )  vs ixs
    in  wrapInMatrix res

matrix1DRep  v = error $  "matrix1DRep " ++  (show . pretty) v


partitionMSetOfSetsRep :: VarData -> E
partitionMSetOfSetsRep VarData{vEssence=[xMatch| vs :=  value.matrix.values |]} =
    let parts = map toPart vs 
    in  [xMake| value.partition.values := parts |]

    where 
    toPart [xMatch| es := value.matrix.values |] =  [xMake| part := es |]


repSelector :: [Tree String] -> Tree String
repSelector arr = arr !! (length arr -1)


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

