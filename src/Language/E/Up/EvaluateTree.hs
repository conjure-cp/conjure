{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}

module Language.E.Up.EvaluateTree (
    evalTree
) where

import Language.E

import Language.E.Up.Data
import Language.E.Up.Common(transposeE,matrixToTuple,unwrapExpr,wrapInExpr)
import Language.E.Up.Debug

import qualified Data.Map as M

--TODO should really use a Trie like Structure

evalTree :: M.Map String VarData ->  Tree String  -> (String,E)
evalTree mapping (Branch name arr) =
    (name,evalTree' mapping [name] (repSelector arr))

evalTree mapping tree@(Leaf name) =
    (name,  evalTree' mapping []  tree)

evalTree _ _ = _bugg "evalTree no match"

evalTree' :: M.Map String VarData -> [String] -> Tree String  -> E
evalTree' mapping prefix (Leaf part) =
    repConverter part vdata

    where
    name    = intercalate "_" (prefix ++ [part])
    lookUpE = fromMaybe (_bugg "fromMaybe: lookUpE evalTree'")  . flip M.lookup mapping
    vdata   = lookUpE  name


evalTree' mapping prefix (Tuple arr) =
    let items =  map (unwrapExpr . evalTree' mapping prefix ) arr
    in  [xMake| expr.value.tuple.values := items |]

{-
evalTree' mapping prefix (Branch s@"Explicit" arr) =

    errr (prefix,arr,same)
    where
    same =  M.filterWithKey filterer mapping
    filterer k _ = (intercalate "_" $ prefix ++ [s] ) `isPrefixOf` k
-}


evalTree' mapping prefix (Branch s@"AsReln"  arr) =
    let res = evalTree' mapping (prefix ++ [s])  (repSelector arr)
    in relnToFunc res
     `_p` ("evalTree' AsReln", [unwrapExpr res] )

evalTree' mapping prefix (Branch s@"Matrix1D" arr) =
    let res = evalTree' mapping (prefix ++ [s])  (repSelector arr)
        indexArr = indexe !! (length prefix -1)
        converted = matrix1DRep [indexArr] res
    in converted
        `_p` ("evalTree' indexArr" ++ groom indexArr, [converted])
        `_p` ("evalTree' m1d res", [res])

    where
    name  = intercalate "_" $ prefix ++ s : getName (repSelector arr)
    VarData{vIndexes=indexe} =
        fromMaybe (_bugg "fromMaybe evalTree': Matrix1D") (M.lookup name mapping)

    getName :: Tree String  -> [String]
    getName (Leaf s2) = [s2]
    getName (Branch s2 arr2) = s2 : getName (repSelector arr2)
    getName (Tuple arr2) = getName (head arr2)


evalTree' mapping prefix (Branch s@"MSetOfSets"  arr) =
    let res = evalTree' mapping (prefix ++ [s])  (repSelector arr)
    in matrixToPartiton res

evalTree' mapping prefix (Branch s@"ExplicitVarSize" [t@(Tuple _) ])  =
    let t' =  evalTree' mapping (prefix ++ [s]) t
        (t1,t2) = tupleToArr t'
        mt = mergeExplicitVarSizeTuple t1 t2
    in  mt
        -- `_e` ("evalTree' ExplicitVarSize", [mt])
        -- `_e` ("evalTree' t2", [t2])
        -- `_e` ("evalTree' t1", [t1])

    where
    tupleToArr :: E -> (E,E)
    tupleToArr [xMatch| [a,b] := expr.value.tuple.values |] = (a,b)
    tupleToArr _ = _bugg "tupleToArr"

evalTree' mapping prefix (Branch part arr) =
    evalTree' mapping (prefix ++ [part])  (repSelector arr)


repSelector :: [Tree String] -> Tree String
repSelector arr = arr !! (length arr -1)

matrixToPartiton :: E -> E
matrixToPartiton [xMatch| vs := expr.value.matrix.values|] =
    let res = map func vs
    in  [xMake| expr.value.partition.values := res |]

    where
    func [xMatch| xs := value.matrix.values |] = [xMake| part := xs |]
    func f = _bug "matrixToPartiton func" [f]

matrixToPartiton e = _bug "matrixToPartiton" [e]

relnToFunc :: E -> E
relnToFunc [xMatch| [v] := expr|] =
    let res = relnToFunc (reTuple v)
    in  [xMake| expr := [res] |]
     `_p` ("evalTree' relnToFunc res", [res] )



relnToFunc [xMatch| vals := value.matrix.values.value.tuple |] =
    let vals' = map f vals
        res   = [xMake| value.function.values := vals' |]
    in res

    where 
    f (Tagged "values" arr) =  [xMake| mapping := arr |]
    f e = _bugg "relnToFunc f" [e]

relnToFunc [xMatch| _  := value.matrix.values.value.matrix.values
                  | vs := value.matrix.values |] =
    let res = map relnToFunc vs
    in  [xMake| value.matrix.values := res |]


-- CHECK with muti-dim matrixes
relnToFunc [xMatch| _  := value.matrix.values.value.matrix.values.value.tuple.values
                  | vs := value.matrix.values |] =
    let res = map relnToFunc vs
    in  [xMake| value.matrix.values := res |]

relnToFunc e =
    let tupled = reTuple e
    in  relnToFunc tupled

-- Converts the most nested matrix to a tuple
reTuple :: E -> E
reTuple e@[xMatch| _  := value.tuple.values.value.literal |] = e

reTuple [xMatch| _  := value.matrix.values.value.literal
               | vs := value.matrix |]  =
    [xMake| value.tuple := vs |]

reTuple [xMatch| vs := value.matrix.values |]  =
    let res = map reTuple vs
    in  [xMake| value.matrix.values := res |]

reTuple e = _bug "reTuple" [e]


repConverter ::  String -> VarData -> E
repConverter  kind  vdata@VarData{vEssence = es} =
    case kind of
      "Explicit"   -> explicitRep vdata
      "Occurrence" -> occurrenceRep vdata
      "Matrix1D"   -> matrix1DRep (vIndexes vdata) (vEssence vdata)
      "ExplicitVarSizeWithDefault" -> explicitVarSizeWithDefaultRep vdata
      _            -> es

matrix1DRep ::  [[Integer]] -> E -> E
matrix1DRep indexe [xMatch| [v] := expr |]  =
    let res = matrix1DRep indexe v
    in  [xMake| expr := [res] |]

matrix1DRep [ix] [xMatch| vals := value.matrix.values |] =
    let togther = zip ix vals
        result  = map (\(a,b) -> [xMake| mapping := [ func a,  b] |] ) togther
        result' = [xMake| value.function.values := result |]
    in  result'

    where func a = [xMake| value.literal := [ Prim (I a) ] |]

matrix1DRep (_:xs) [xMatch| vs := value.matrix.values |]  =
    let arr = map (matrix1DRep xs) vs
    in [xMake| value.matrix.values := arr |]

-- for function whoes ranges are tuples  e.g setOfFuncTupleSet and setOfFuncTupleSetMatrix
matrix1DRep [ix] e@[xMatch| _ := value.tuple.values |] =
    let mappings = zipWith (\a b -> [xMake| mapping := [ func a,  b] |]) ix vals
    in  [xMake| value.function.values := mappings |]
        `_p` ("Matrix1D range", [e])

    where
    func a = [xMake| value.literal := [ Prim (I a) ] |]

    -- TODO number of matrix taken off should be releated to number of
    -- sets/matrixes the func and remove and record them
    vals  =  (map matrixToTuple . transposeE . convert) e
    -- take a matrix off each nested matrix
    convert :: E -> [E]
    convert [xMatch| vs := value.tuple.values |] =
        map convert' vs

        where
        convert' :: E -> E
        convert' [xMatch| vs2  := value.tuple.values |]  =
            let res = map convert' vs2
            in  [xMake| value.tuple.values := res |]
        convert' f@[xMatch| _ := value.matrix.values |]  = f
        convert' f = _bug "Matrix1DRep convert'" [f]

    convert f = _bug  "Matrix1DRep convert" [f]


matrix1DRep ix e =
    _bugi "Matrix1DRep not Handled (indexes, e):" (ix, [e])


explicitVarSizeWithDefaultRep :: VarData -> E
explicitVarSizeWithDefaultRep  VarData{vBounds=b, vEssence=e} =
   wrapInExpr $ explicitVarSizeWithDefault (head b) (unwrapExpr e)

explicitVarSizeWithDefault :: Integer -> E -> E
explicitVarSizeWithDefault toRemove [xMatch| vs := value.matrix.values  
                                           | _  := value.matrix.values.value.literal |] =
    let vs' = filter f vs
    in  [xMake| value.matrix.values := vs' |]

    where 
    f [xMatch| [Prim (I i)] := value.literal  |] = i /= toRemove
    f e = _bug "explicitVarSizeWithDefault f" [e]

explicitVarSizeWithDefault toRemove [xMatch| vs := value.matrix.values |] =
    let vs' =   map (explicitVarSizeWithDefault toRemove) vs
    in  [xMake| value.matrix.values := vs' |]

explicitVarSizeWithDefault toRemove e = _bugi "explicitVarSizeWithDefault" (toRemove,[e])

occurrenceRep :: VarData -> E
occurrenceRep  VarData{vIndexes = ix,
                vEssence=[xMatch| [Tagged _ [Tagged "values" _ ] ] := expr.value
                                | lits := expr.value.matrix.values.value.literal |]}  =
    let set = Tagged "matrix" [Tagged "values" (occurrence' ix lits)]
    in [xMake| expr.value := [set] |]

occurrenceRep  VarData{vIndexes = ix,
                vEssence=[xMatch| [Tagged _ [Tagged "values" mat_val] ] := expr.value |]}  =
    let res =  map (occurrence (tail ix)) mat_val
        set = Tagged "matrix" [Tagged "values" res ]

    in  [xMake| expr.value := [set] |]

occurrenceRep _ = _bugg "occurrenceRep"

occurrence :: [[Integer]] -> E -> E
occurrence ix [xMatch| [Tagged "matrix" _ ] := value
             | lits := value.matrix.values.value.literal |]  =
   let set_val  =  occurrence' ix lits
   in [xMake| value.matrix.values :=  set_val |]

occurrence ix [xMatch| [Tagged "matrix" _ ] := value
             |  mats := value.matrix.values |] =
    let vals =  map (occurrence (tail ix)) mats
    in [xMake| value.matrix.values :=  vals |]

occurrence ix f = _bugi "occurrence" (ix,[f])

occurrence' :: [[Integer]] -> [E] -> [E]
occurrence' ix lits=
   let all_vals =  zip lits (head ix)
       in_set   = filter f all_vals
   in  map (\(_,n) ->  [xMake| value.literal :=  [Prim (I n)] |] ) in_set

   where 
   f (Prim(B b),_) =  b 
   f (Prim(I b),_) =  b == 1  -- Leaving this here so I don't have recreate my tests
   f t = _bugi "occurrence' f" (t,[]) 

explicitRep :: VarData -> E
explicitRep VarData{vEssence=[xMatch| [Tagged _ vals ] := expr.value
                                    | _ := expr.value |]}  =
    let set = Tagged "matrix" vals in
    [xMake| expr.value := [set] |]

explicitRep _ = _bugg "explicitRep"

-- e.g [ [2,2], [4,4], [6,7]] -> [ [2,4,6], [2,4,7] ]
transpose :: [[t]] -> [[t]]
transpose []     = []
transpose [xs]   = map (: [])  xs
transpose (x:xs) = zipWith (:) x (transpose xs)

-- Basically does what pairr was originally written for
-- which is to  convert
-- [[2, 3], [2, 3], [2, 3], [2, 3]]  and [[4, 4], [4, 4], [4, 4], [7, 4]])
-- to [[[2, 4], [3, 4]], [[2, 4], [3, 4]], [[2, 4], [3, 4]], [[2, 7], [3, 4]]])
-- by pairring the elements of the two matrixes
pairMatrix :: [E] -> E
pairMatrix arr =
    let res = foldl1 pairMatrix' arr
    in  res
    -- `_b` ("pairMatrix main args",arr)
    -- `_b` ("pairMatrix main res",[res])

pairMatrix' :: E -> E -> E
pairMatrix'
    [xMatch| v1 := value.matrix.values |]
    [xMatch| v2 := value.matrix.values |] =
    let res' = pairEqual v1  v2

    in  case res' of
        ([xMatch| _ := value |]:_)  -> -- __j "pairMatrix' pres1"
            [xMake| value.matrix.values := res'|]
        ([xMatch| _ := values |]:_) -> -- __j "pairMatrix' pres2"
            [xMake| value.matrix.values := (concatMap con res')|]

        f -> _bug "pairMatrix'" f

    where con [xMatch| vs := values |] = vs
          con e = _bug "pairMatrix' con" [e]

pairMatrix' a@[xMatch| _ := value.literal |]
       b@[xMatch| _ := value.literal |] =
    -- __j "pairMatrix' lit res" $
        [xMake| value.matrix.values :=  [a,b] |]
        -- `_e` ("pairMatrix' lit",[a,b])

pairMatrix' a b = _bug "pairMatrix" [a,b]


pairEqual :: [E] -> [E] -> [E]
pairEqual v1 v2
    |  l1 == l2   =
            let zipped = zipWith (\a b -> [a, b] ) v1 v2
                res = map pairMatrix zipped
            in res
    where l1 = length v1
          l2 = length v2

pairEqual _ _ = _bugg "pairEqual not equal lengths"

mergeExplicitVarSizeTuple :: E -> E -> E
mergeExplicitVarSizeTuple
    ([xMatch| v1  := value.matrix.values.value.literal|])
    ([xMatch| v2s := value.tuple.values.value.matrix|]) =
        let vals   = map f1 v2s
            chosen = map (filter f2) vals
            rep    = transpose . map (map snd) $ chosen
            ans    = map (\a -> [xMake| value.tuple.values := a|]) rep
            ans'   = [xMake| expr.value.matrix.values := ans|]
        in  ans'
            `_e` ("mergeExplicitVarSizeTuple ans'",[ans'])

    where
    f1 (Tagged Tvalues arr) =   zip v1 arr
    f1 e = _bug "mergeExplicitVarSizeTuple f1" [e]
    f2 (Prim (I i),_)       = i == 1
    f2 (e,t) = _bugi "mergeExplicitVarSizeTuple f1" (t,[e])

mergeExplicitVarSizeTuple
    e1@([xMatch| [Tagged "values" v1] := value.matrix|])
    e2@([xMatch| [Tagged "values" v2] := value.matrix|])
    = let (dim1, dim2) = (dimOfMatrix e1, dimOfMatrix e2) in
        if (dim1 == dim2) && dim1 == 1 then
             let chosen = selected (e1,e2) in
             [xMake| expr :=  [chosen] |]
        else if dim1 == dim2 then
          let togther = zip v1 v2
              chosen  = map selected togther in
              [xMake| expr.value.matrix :=  [Tagged "values" chosen] |]
        else if dim1 < dim2 then
             let chosen =  selected (e1, e2) in
             [xMake| expr :=  [chosen] |]
        else
            _bugg "t1's has to be <= t2's dim"
    where
        dimOfMatrix :: E -> Integer
        dimOfMatrix [xMatch| v := value.matrix.values
                    |  _  := value.matrix.values.value.matrix |] = 1 + dimOfMatrix (head v)
        dimOfMatrix [xMatch| _ := value.matrix.values |] = 1
        dimOfMatrix f = _bug "dimOfMatrix" [f]

mergeExplicitVarSizeTuple
    e1@([xMatch| _  := value.matrix.values.value.matrix
               | _  := value.matrix.values|])
    e2@([xMatch| _  := value.tuple.values.value.matrix
               | ts := value.tuple.values|]) =
    let res = map (unwrapExpr . mergeExplicitVarSizeTuple  e1) ts
        paired = pairMatrix res
    in [xMake| expr := [paired]|]
        `_p` ("mergeExplicitVarSizeTuple M T ",[paired])
        `_p` ("mergeExplicitVarSizeTuple M T e2",[e2])
        `_p` ("mergeExplicitVarSizeTuple M T e1",[e1])


mergeExplicitVarSizeTuple f g = _bug "mergeExplicitVarSizeTuple" [f,g]


selected :: (E,E) -> E
selected ([xMatch| _ := value.matrix.values.value.literal
                 | [Tagged "values" v1] := value.matrix |]
        ,[xMatch| [Tagged "values" v2] := value.matrix |])=
            let togther = zip v1 v2
                chosen  = filter (\(a,_) -> getValue a) togther
                values  = map snd chosen in
        [xMake| value.matrix :=  [Tagged "values" values] |]

selected ([xMatch| [Tagged "values" v1] := value.matrix |]
         ,[xMatch| [Tagged "values" v2] := value.matrix |])=
            let togther = zip v1 v2
                values  = map selected togther  in
        [xMake| value.matrix :=  [Tagged "values" values] |]

selected _ = _bugg "selected"

getValue :: E -> Bool
getValue value =
    case value of
        [xMatch| [Prim (I n)] := value.literal |] ->  n == 1
        [xMatch| [Prim (B b)] := value.literal |] ->  b
        f ->  _bug "getValue" [f]


_bug :: String -> [E] -> t
_bug  s = upBug  ("EvaluateTree: " ++ s)
_bugi :: (Show a) => String -> (a, [E]) -> t
_bugi s = upBugi ("EvaluateTree: " ++ s )
_bugg :: String -> t
_bugg s = _bug s []

