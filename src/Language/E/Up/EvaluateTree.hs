{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}

module Language.E.Up.EvaluateTree (
    evalTree
) where

import Language.E

import Language.E.Up.Data
import Language.E.Up.Common(transposeE,matrixToTuple,unwrapExpr)
import Language.E.Up.Debug

import qualified Data.Map as M


evalTree :: M.Map String VarData ->  Tree String  -> (String,E)
evalTree mapping (Branch name arr) =
    (name,evalTree' mapping [name] (repSelector arr))

evalTree mapping tree@(Leaf name) =
    (name,  evalTree' mapping []  tree)


evalTree' :: M.Map String VarData -> [String] -> Tree String  -> E
evalTree' mapping prefix (Leaf part) =
    repConverter part vdata

    where
    name    = intercalate "_" (prefix ++ [part])
    lookUpE = fromMaybe (error "fromMaybe et: lookUpType")  . flip M.lookup mapping
    vdata   = lookUpE  name


evalTree' mapping prefix (Tuple arr) =
    let items =  map (unwrapExpr . evalTree' mapping prefix ) arr
    in  [xMake| expr.value.tuple.values := items |]

evalTree' mapping prefix (Branch s@"AsReln"  arr) =
    let res = evalTree' mapping (prefix ++ [s])  (repSelector arr)
    in relnToFunc res
     `_p` ("evalTree' AsReln", [unwrapExpr res] )

evalTree' mapping prefix (Branch s@"Matrix1D" arr) =
    let res = evalTree' mapping (prefix ++ [s])  (repSelector arr)
        indexArr = indexes !! (length prefix -1)
        converted = matrix1DRep [indexArr] res
    in converted
    {-in errpM ("indexArr" ++ groom indexArr) [converted]-}
        `_p` ("indexArr" ++ groom indexArr, [converted])

    where
    name  = intercalate "_" $ prefix ++ s : getName (repSelector arr)
    VarData{vIndexes=indexes} =
        fromMaybe (error "fromMaybe evalTree': Matrix1D") (M.lookup name mapping)

    getName :: Tree String  -> [String]
    getName (Leaf s) = [s]
    getName (Branch s arr) = s : getName (repSelector arr)
    getName (Tuple arr) = getName (head arr)


evalTree' mapping prefix (Branch s@"SetOfSets"  arr) =
    let res = evalTree' mapping (prefix ++ [s])  (repSelector arr)
    in matrixToPartiton res

evalTree' mapping prefix (Branch s@"ExplicitVarSize" [t@(Tuple _) ])  =
    let t' =  evalTree' mapping (prefix ++ [s]) t
        [t1,t2] = tupleToArr t'
        mt = mergeExplicitVarSizeTuple t1 t2
    in  mt
        -- `_e` ("evalTree' ExplicitVarSize", [mt])
        -- `_e` ("evalTree' t2", [t2])
        -- `_e` ("evalTree' t1", [t1])

    where
    tupleToArr :: E -> [E]
    tupleToArr [xMatch| arr := expr.value.tuple.values |] = arr

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

relnToFunc :: E -> E
relnToFunc e@[xMatch| [v] := expr|] =
    let res = relnToFunc (reTuple v)
    in  [xMake| expr := [res] |]
     `_p` ("evalTree' relnToFunc res", [res] )



relnToFunc [xMatch| vals := value.matrix.values.value.tuple |] =
    let vals' = map (\(Tagged "values" arr) -> [xMake| mapping := arr |] ) vals
        res   = [xMake| value.function.values := vals' |]
    in res

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


repConverter ::  String -> VarData -> E
repConverter  kind  vdata@VarData{vEssence = es} =
    case kind of
      "Explicit"   -> explicitRep vdata
      "Occurrence" -> occurrenceRep vdata
      "Matrix1D"   -> matrix1DRep (vIndexes vdata) (vEssence vdata)
      _            -> es

matrix1DRep ::  [[Integer]] -> E -> E
matrix1DRep indexes [xMatch| [v] := expr |]  =
    let res = matrix1DRep indexes v
    in  [xMake| expr := [res] |]

matrix1DRep [ix] [xMatch| vals := value.matrix.values |] =
    let togther = zip ix vals
        result  = map (\(a,b) -> [xMake| mapping := [ func a,  b] |] ) togther
        result' = [xMake| value.function.values := result |]
    in  result'
    -- in erri (ix,vals)

    where func a = [xMake| value.literal := [ Prim (I a) ] |]

matrix1DRep (x:xs) [xMatch| vs := value.matrix.values |]  =
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
        convert' e@[xMatch| vs  := value.tuple.values |]  =
            let res = map convert' vs
            in  [xMake| value.tuple.values := res |]
        {-convert' [xMatch| [singleton] := value.matrix.values |]  = singleton-}
        convert' e@[xMatch| _ := value.matrix.values |]  = e 


matrix1DRep ix e =
    erriM "Matrix1DRep not Handled (indexes, e):" (ix, [e])


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


occurrence :: [[Integer]] -> E -> E
occurrence ix [xMatch| [Tagged "matrix" _ ] := value
             | lits := value.matrix.values.value.literal |]  =
   let set_val  =  occurrence' ix lits
   in [xMake| value.matrix.values :=  set_val |]

occurrence ix [xMatch| [Tagged "matrix" _ ] := value
             |  mats := value.matrix.values |] =
    let vals =  map (occurrence (tail ix)) mats
    in [xMake| value.matrix.values :=  vals |]

occurrence' :: [[Integer]] -> [E] -> [E]
occurrence' ix lits=
   let all_vals =  zip lits (head ix)
       in_set   = filter (\(Prim(I b),_) -> b == 1) all_vals
   in  map (\(_,n) ->  [xMake| value.literal :=  [Prim (I n)] |] ) in_set


explicitRep :: VarData -> E
explicitRep VarData{vEssence=[xMatch| [Tagged _ vals ] := expr.value
                                    | _ := expr.value |]}  =
    let set = Tagged "matrix" vals in
    [xMake| expr.value := [set] |]


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

    where con [xMatch| vs := values |] = vs
          con e = errb [e]

pairMatrix' a@[xMatch| _ := value.literal |]
       b@[xMatch| _ := value.literal |] =
    -- __j "pairMatrix' lit res" $
        [xMake| value.matrix.values :=  [a,b] |]
        -- `_e` ("pairMatrix' lit",[a,b])

pairMatrix' a b = errt [a,b]


pairEqual :: [E] -> [E] -> [E]
pairEqual v1 v2
    |  l1 == l2   =
            let zipped = zipWith (\a b -> [a, b] ) v1 v2
                res = map pairMatrix zipped
            in res
    where l1 = length v1
          l2 = length v2


mergeExplicitVarSizeTuple :: E -> E -> E
mergeExplicitVarSizeTuple
    ([xMatch| v1  := value.matrix.values.value.literal|])
    ([xMatch| v2s := value.tuple.values.value.matrix|]) =
        let vals   = map (\(Tagged Tvalues arr)  -> zip v1 arr) v2s
            chosen = map (filter (\(Prim (I i),_) -> i ==1)) vals
            rep    = transpose . map (map snd) $ chosen
            ans    = map (\a -> [xMake| value.tuple.values := a|]) rep
            ans'   = [xMake| expr.value.matrix.values := ans|]
        in  ans'
            `_e` ("mergeExplicitVarSizeTuple ans'",[ans'])
-- old
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
            error "t1's has to be <= t2's dim"
    where
        dimOfMatrix [xMatch| v := value.matrix.values
                    |  _  := value.matrix.values.value.matrix |] = 1 + dimOfMatrix (head v)
        dimOfMatrix [xMatch| _ := value.matrix.values |] = 1

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


selected :: (E,E) -> E
selected ([xMatch| _ := value.matrix.values.value.literal
                 | [Tagged "values" v1] := value.matrix |]
        ,([xMatch| [Tagged "values" v2] := value.matrix |]))=
            let togther = zip v1 v2
                chosen  = filter (\(a,_) -> getValue a == 1) togther
                values  = map snd chosen in
        [xMake| value.matrix :=  [Tagged "values" values] |]

selected ([xMatch| [Tagged "values" v1] := value.matrix |]
         ,[xMatch| [Tagged "values" v2] := value.matrix |])=
            let togther = zip v1 v2
                values  = map selected togther  in
        [xMake| value.matrix :=  [Tagged "values" values] |]


getValue :: E -> Integer
getValue value =
    case value of
        [xMatch| [Prim (I n)] := value.literal |] ->  n


