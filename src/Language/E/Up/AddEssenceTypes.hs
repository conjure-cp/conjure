{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}

module Language.E.Up.AddEssenceTypes(
    toEssenceRep
    ,wrapInMatrix
    ,unwrapMatrix
    ,flattenInt
)where

import Language.E

import Language.E.Up.Data
import Language.E.Up.Debug
import Language.E.Up.Common(transposeE,unwrapMatrix,matrixToTuple,unwrapExpr)

import Data.Maybe


singleMatrixOrTuple :: E -> Bool
singleMatrixOrTuple [xMatch| [_] := value.matrix.values |] = True
singleMatrixOrTuple [xMatch| _   := value.tuple  |] = True
singleMatrixOrTuple _ = False

matrixOrTuple :: E -> Bool
matrixOrTuple [xMatch| _ := value.matrix |] = True
matrixOrTuple [xMatch| _ := value.tuple  |] = True
matrixOrTuple _ = False

unwrapTuple :: E -> [E]
unwrapTuple [xMatch| vs := value.tuple.values|] = vs
unwrapTuple e = upBug "AddEssenceTypes: unwrapTuple failed" [e]

unwrapValues :: E -> [E]
unwrapValues [xMatch| vs := values |] = vs
unwrapValues e = upBug "AddEssenceTypes: unwrapValues failed" [e]


unwrapSingleMatrix :: E -> E
unwrapSingleMatrix [xMatch| [vs] := value.matrix.values |] = vs
unwrapSingleMatrix e  = e

wrapInMatrix :: [E] -> E
wrapInMatrix arr = [xMake| value.matrix.values := arr |]

wrapInTuple :: [E] -> E
wrapInTuple arr = [xMake| value.tuple.values := arr |]

wrapInExpr :: [E] -> E
wrapInExpr arr = [xMake| expr := arr |]

isMatrix :: E -> Bool
isMatrix [xMatch| _ := value.matrix.values |] = True
isMatrix _ = False

isTagMatrix :: TagT -> Bool
isTagMatrix (TagSingle "matrix") = True
isTagMatrix _ = False

isTagTuple :: TagT -> Bool
isTagTuple (TagTuple _) = True
isTagTuple _ = False

isNestedTuple :: [TagT] -> E ->  Maybe (Int, [TagT], E)
isNestedTuple [ TagTuple [  ts@(TagSingle _:_) ] ]
              [xMatch| [vs2] := value.tuple.values|] =
    Just (1,ts,vs2)
        `_k` ("isNestedTuple fin",(ts,[vs2]) )


isNestedTuple [ TagTuple [[t]] ]  [xMatch| [vs2] := value.tuple.values|] =
    case  isNestedTuple [t] vs2 of
      Nothing        -> Nothing
      Just (i,t',_e) -> Just (i+1,t',_e)

    `_k` ("isNestedTuple`",(t,[vs2]) )

isNestedTuple r@[ TagTuple t ]  e@[xMatch| vs := value.tuple.values |]
    | all (tagAllowed . head ) t && all isMatrix vs  =

    Just (0,r, e)
    `_k` ("isNestedTuple3",(t,vs) )


    where
    tagAllowed :: TagT -> Bool
    tagAllowed (TagSingle "matrix") = True
    tagAllowed (TagSingle _)        = True
    tagAllowed _                    = False

isNestedTuple _ _ = Nothing
{-isNestedTuple ts e = erri (ts,[e])-}


reTuple :: Int -> E -> E
reTuple  0 e = e
reTuple  n e = reTuple (n-1) [xMake| value.tuple.values := [e]|]


getLit :: E -> Maybe E
getLit [xMatch| [ele] := value.matrix.values |] =  getLit ele
getLit f@[xMatch| _ := value.literal |]  = Just f
getLit _ = Nothing


toEssenceRep :: [TagT] -> E -> E

-- To handles singeton tuples
-- FIXME rec handling
toEssenceRep [TagSingle "matrix", TagTuple [[ts]] ]
             [xMatch| [vs1] := value.tuple.values|]
    | isJust val =
       let  (count,ts',e') = (fromJust val)
            res            = toEssenceRep  (TagSingle "matrix": ts')  e'
            wrapped        = map (reTuple (count+1)) (unwrapMatrix res)
       in [xMake| value.matrix.values := wrapped |]
        `_p` ("singleton wrapped", [wrapped])
        `_p` ("singleton res", [res])
        `_p` ("singleton e'", [e'])
        `_f` ("singleton ts'", ts')
        `_g` ("singleton count", count)
        `_p` ("singleton vs1", [vs1])
        `_f` ("singleton ts", [ts])

    where
    val = isNestedTuple [ts] vs1


toEssenceRep r@[TagTuple _]
             e@[xMatch| _ := value.tuple.values|]
    | isJust info && False =

    let (ts',e') = fromJust info
        res  = toEssenceRep (TagSingle "matrix" : ts') e'
        res' = map (\f -> [xMake| value.tuple.values := [f] |]) (unwrapMatrix res)
        {-res' = [xMake| value.tuple.values := [res] |]-}

    in wrapInMatrix res'
    {-in  error "allowed"-}

    `_p` (" MT Nested tuples res", [res'])
    `_p` (" MT Nested tuples res", [res])
    `_i` (" MT Nested tuples info", (ts',[e'])  )
    `_p` (" MT Nested tuples e", [e])
    `_f` (" MT Nested tuples r",r)

    where
    info = isAllowed r e

    isAllowed :: [TagT] -> E -> Maybe ([TagT],E)
    isAllowed [TagTuple [_ts]]  [xMatch|  [vs]:= value.tuple.values|] =
        Just (_ts,vs)

    isAllowed _ _ = Nothing


-- This really should not be needed but handles stuff wrapped in a values
toEssenceRep tags@[TagSingle "matrix", TagTuple _]
    [xMatch| vs := values.value.tuple.values
             | [v] := values |]
    | all matrixOrTuple vs =
       let res = toEssenceRep tags v
           res' = [xMake| values := [res] |]
       in  res'
       `_p` ("M T values.value.tuple.values", [res])


-- fix NB T value.tuple  first which should be done now
-- Idea is to rec handle tags until  len ts = len vs
-- see tmm4
-- toEssenceRep tags@[TagSingle "matrix", TagTuple ts] is

toEssenceRep tags@[TagSingle "matrix", TagTuple [[TagSingle "matrix", TagTuple ts ]] ]
    [xMatch| vs  := value.tuple.values .value.tuple.values
           | [_] := value.tuple.values |]
    | all matrixOrTuple vs =
    let vs2  = zipWith handleNested ts vs
        res2 = map unwrapMatrix (transposeE vs2)
        res3 = map transposeE res2
        res4 = map (wrapInMatrix . map matrixToTuple) res3

    in [xMake| value.matrix.values.value.tuple.values := res4 |]
    {-in error "d"-}

        `_p` ("P T tuple tuple res4",res4)
        `_p` ("P T tuple tuple res3",res3)
        `_p` ("P T tuple tuple res2",res2)
        `_p` ("P T tuple tuple vs",vs)
        `_p` ("P T tuple tuple vs2",vs2)
        `_p` ("P T tuple tuple vs",vs)
        `_f` ("P T tuple tuple ts",ts)
        `_f` ("P T tuple tuple tags",tags)


    where

    handleNested ::  [TagT] -> E -> E
    handleNested ts1 e@[xMatch| _ := value.tuple.values |] =

        let ts2 = TagSingle "matrix" : ts1
            rs  = toEssenceRep ts2 e
            rs2 = after ts2 rs

        in rs2
        {-in errb [rs2]-}
            `_p` ("  P T handleNerted res2",[rs2])
            `_f` ("  P T handleNested ts",ts2)
            `_p` ("  P T handleNerted res",[rs])
            `_p` ("  P T handleNerted e",[e])
            `_f` ("  P T handleNested ts",ts2)

    handleNested _ e = e  `_p` (" P T handleNerted unchanged",[e])

    after :: [TagT] -> E -> E
    after ts1 [xMatch| vs1 := value.matrix.values |] =
        let vss =  map (toEssenceRep ts1) vs1
        in  [xMake| value.matrix.values := vss |]

    after _ e1 = e1
    {-after ts e = erri (ts, [e])-}

-- see _matrix_of_tuples
toEssenceRep tags@[TagSingle "matrix", TagTuple ts]
    e@[xMatch| vs := value.tuple.values |]
    | all singleMatrixOrTuple vs =

    let res  = zipWith func ts vs
        res' = [xMake| value.matrix.values.value.tuple.values := res |]

    in res'
    `_p` ("M T value.tuple.values res'",[res'])
    `_p` ("M T value.tuple.values res",[res])
    `_p` ("M T value.tuple.values vs" ++ show (length vs) ,vs)
    `_p` ("M T value.tuple.values args",[e])
    `_f` ("M T value.tuple.values ts",ts)
    `_f` ("M T value.tuple.values tags",tags)

    where

    func :: [TagT] ->  E -> E

    -- FIXME this method is seems to work by unwrapping the singleton matrix of a int
    -- but this should not be really needed
{-
func [TagSingle "int"]  [xMatch| [ele] := value.matrix.values |]
        | isJust res = fromJust res
            `_p` ("T2 unwraping int", [res])
        where res = getLit ele
-}
    func ts2  [xMatch| [ele] := value.matrix.values |] = ele
        `_p` ("M T func mat", [ele])
        `_f` ("M T func mat ts",ts2)

    func r@[TagTuple ts1]   e1@[xMatch| vs1 := value.tuple.values |] =
        let
            vs2 = map unwrapSingleMatrix vs1
            vs3 = zipWith handle ts1 vs2
        in wrapInTuple vs3

        `_p` (" T2 func vs3",vs3)
        `_p` (" T2 func vs2",vs2)
        `_p` (" T2 func e",[e1])
        `_f` (" T2 func ts",ts1)
        `_f` (" T2 func r",r)

        where
        handle :: [TagT] -> E -> E
        handle _r@[TagTuple _]  _e@[xMatch| _ := value.tuple.values.value.matrix |] =
            let res =  toEssenceRep (TagSingle "matrix" : _r)  _e
                res' = unwrapSingleMatrix res
            in res'
            `_p` (" T2 handle res'",[res'])
            `_p` (" T2 handle e",[_e])
            `_f` (" T2 handle r",_r)

        -- FIXME this method is seems to work by unwraping the singeton matrix of a int
        -- but this should not be really needed
{-
        handle [TagSingle "int"]  [xMatch| [ele] := value.matrix.values |]
            | isJust res = fromJust res
                `_p` ("T2 unwraping int", [res])
            where res = getLit ele
-}

        handle _ts _e = _e
            `_k` ("t2 not handled", (_ts,[_e]))

    -- FIXME for o92 /o9
    -- removed the matrix around ts1 with no ill and postive effect
    func ts1  e1@[xMatch| vs1 := value.tuple.values |] =
        let
            vs2 = map unwrapSingleMatrix vs1
            e2  = [xMake| value.tuple.values := vs2 |]
            rs  = toEssenceRep ts1 e2
        in  rs
            `_p` ("M T func rs",[rs])
            `_p` ("M T func e2",[e2])
            `_p` ("M T func vs2",vs2)
            `_p` ("M T func e",[e1])
            `_f` ("M T func ts",ts1)

    func _ e1 = upBug "func matrix of tuples" [e1]

toEssenceRep tags@[TagSingle "matrix", TagTuple ts]
    [xMatch| vs := value.tuple.values |]
    | all matrixOrTuple vs =

    let
        vs2  = zipWith handleNested ts vs
        res  = map matrixToTuple (transposeE vs2)
        res' = [xMake| value.matrix.values := res |]

    --in  errb res
    in  res'
    `_p` ("N T value.tuple.values res'",[res'])
    `_p` ("N T value.tuple.values res",[res])
    `_f` ("N T value.tuple.values ts",ts)
    `_p` ("N T value.tuple.values vs2",vs2)
    `_p` ("N T value.tuple.values vs",vs)
    `_f` ("N T value.tuple.values ts",ts)
    `_f` ("N T value.tuple.values tags",tags)

    where
    -- should handle nested tuples e.g tupley26, seems to work
    handleNested ::  [TagT] ->  E -> E
    handleNested ts1@[TagTuple tts]  e@[xMatch| vs1 := value.tuple.values |]
        -- CHECK not sure about using matrixOrTuple or isMatrix
        | all ( not . isTagMatrix . head ) tts &&  all isMatrix vs1 =

        let ts'  = TagSingle "matrix" : ts1
            rs   = toEssenceRep ts' e
            rs2  = after ts' rs

        in rs2
        --in errb [rs2]
            --`_p` (" K T handleNerted res2",[rs2])
            `_f` ("  K T handleNested ts'",ts')
            `_p` ("  K T handleNerted res",[rs])
            `_p` ("  K T handleNerted e",[e])
            `_f` ("  K T handleNested ts'",ts')

    handleNested ts1@[TagSingle "matrix", TagTuple _]
                  e@[xMatch| _ := value.tuple.values
                           | _  := value.tuple.values.value.matrix |] =

        let ts2 = TagSingle "matrix"  : ts1
            rs  = toEssenceRep ts2 e
            rs2 = after ts2 rs

        in rs2
        --in errb [rs2]
            `_p` ("  MM T handleNerted res2",[rs2])
            `_f` ("  MM T handleNested ts",ts1)
            `_p` ("  MM T handleNerted res",[rs])
            `_p` ("  MM T handleNerted e",[e])
            `_f` ("  MM T handleNested ts",ts1)


    handleNested ts1  e@[xMatch| _ := value.tuple.values |] =
        let
            rs   = toEssenceRep ts1 e
            rs2  = after ts1 rs

        in rs2
        --in errb [rs2]
            `_p` (" N T handleNerted res2",[rs2])
            `_f` ("  N T handleNested ts",ts1)
            `_p` ("  N T handleNerted res",[rs])
            `_p` ("  N T handleNerted e",[e])
            `_f` ("  N T handleNested ts",ts1)

    handleNested _ e = e   -- `_p` (" N T handleNerted unchanged",[e])

    after :: [TagT] -> E -> E
    after ts1 [xMatch| vs1 := value.matrix.values |] =
        let vss =  map (toEssenceRep ts1) vs1
        in  [xMake| value.matrix.values := vss |]

    after _ e = e
    --after ts1 e = erri (ts1, [e])

-- FIXME  works. should make recursive
toEssenceRep tags@[TagSingle "matrix", TagSingle "matrix", TagTuple ts]
    e@[xMatch| vs := value.tuple.values |]
    | all matrixOrTuple vs =

    let pre   = zipWith prePro ts vs
        res   = map matrixToTuple (transposeE pre)

        pre2  =  map (zipWith prePro ts . unwrapTuple)  res
        res2  =  map transposeE pre2
        fin   =  map wrapper res2

    in wrapInMatrix fin

    `_p` ("NB T value.tuple fin", fin)
    `_p` ("NB T value.tuple res2", res2)
    `_p` ("NB T value.tuple pre2",pre2)
    `_p` ("NB T value.tuple res",res)
    `_p` ("NB T value.tuple pre",pre)
    `_p` ("NB T value.tuple vs",[e])
    `_f` ("NB T value.tuple tags",tags)

    where

    wrapper :: [E] -> E
    -- for innerMutiMatixMatixTuple and tupley32-8-6  e.g singleton matrix
    wrapper arr | all isLit arr =  wrapInTuple   arr
    wrapper arr = (wrapInMatrix .  map matrixToTupleMaybe)  arr
        `_p` ("NB T wrapper",arr)

    --wrapper arr = errb arr

    matrixToTupleMaybe :: E -> E
    matrixToTupleMaybe [xMatch| vs1 := value.matrix|] = [xMake| value.tuple := vs1 |]
    matrixToTupleMaybe _e = _e `_p` ("matrixToTupleMaybe not matrix", [_e])

    isLit :: E -> Bool
    isLit [xMatch| _ := value.literal  |] = True
    isLit _ = False


    prePro :: [TagT] -> E -> E

    -- for singleton tuples should really merge with other function
    prePro r@[TagTuple [[ts1]] ] e1@[xMatch| [vs1] := value.tuple.values |]
        | isJust val =

        let (count,_,e') = (fromJust val)
            tranposed      = transposeE (unwrapMatrix e')
            wrapped        = map (reTuple (count + 1) ) tranposed

        in  wrapInMatrix wrapped
        {-in error "yes"-}

        `_p` ("prepro wrapped", [wrapped])
        `_p` ("prepro e'", tranposed)
        `_p` ("prepro e'", [e'])
        `_f` ("prepro r", [r])
        `_g` ("prepro count", count)
        `_p` ("prepro e", [e1])
        `_f` ("prepro r", [r])

        where
        val = isNestedTuple [ts1] vs1

    --  seem good  guard for tmm3
    prePro ts1@[TagSingle "matrix", TagTuple _] 
            e1@[xMatch| _ := value.tuple.values.value.matrix.values.value.matrix |] =
        let res = toEssenceRep  (TagSingle "matrix": ts1) e1
        --in  upBug "prePro MMT for tmm3,  needs some work" [res]
        in  res
            --`_p` ("prePro MMT  vs", [e1])
            `_p` ("prePro MMT res", [res])
            `_f` ("prePro MMT ts", ts1)
            `_p` ("prePro MMT vs",  [e1])
            `_f` ("prePro MMT ts", ts1)

    -- for C2 kind of  works but have to run toEssenceRep twice which is bad
    prePro ts1@[TagSingle "matrix", TagTuple _]
            e1@[xMatch| _ := value.tuple.values.value.tuple.values |] =
        let res = toEssenceRep  (TagSingle "matrix": ts1) e1
        in  res
        {-in  error "prePro c2  needs some work" -}
            `_f` ("prePro c2  ts", ts1)
            `_p` ("prePro c2  e1", [e1])
            `_p` ("prePro c2  res", [res])
            `_p` ("prePro c2  e1", [e1])
            `_f` ("prePro c2  ts", ts1)


   -- CHECK not sure if to do after all above?
    prePro r@[TagSingle "matrix", TagTuple _]
           e1@[xMatch| _ := value.matrix.values.value.tuple.values.value.matrix |] =
         let res = toEssenceRep ( TagSingle "matrix" : r) e1
         in res

        `_p` ("prePro CT res", [res])
        `_f` ("prePro CT ts", r)
        `_p` ("prePro CT vs",  [e1])
        `_f` ("prePro CT ts", r)

    -- FIXME  fixes c1
    -- ints more nested then they should be
    prePro r@[TagTuple tss] f@[xMatch| vs2 := value.tuple.values |] =
        let res   = zipWith (\z -> toEssenceRep (TagSingle "matrix":z)  ) tss vs2
            res2  = map (\h -> wrapInMatrix [h]) res
            res'  = [xMake| value.tuple.values := res |]
        in  res'
        `_p` ("prePro TV res'", [res'])
        `_p` ("prePro TV res", [res])
        `_p` ("prePro TV e",  [f])
        `_f` ("prePro TV ts", r)

    --prePro ts e = erri (ts, [e])
    prePro ts2 f = f `_i` ("prePro no change", (ts2, [f]))

-- see _tuples_of_matrix
toEssenceRep [TagTuple [  [TagSingle "matrix", TagTuple ts ] ]  ]
    [xMatch| vs  := tuple.values.value.tuple.values
           | [_] := tuple.values |]
    | all matrixOrTuple vs =
        let
            vs2  = zipWith handleNested ts vs
            res  = map matrixToTuple (transposeE vs2)
            res' = [xMake| tuple.values.value.matrix.values := res |]

        in res'
            `_p` ("T M tuple.values.value.tuple.values res", res)
            `_p` ("T M tuple.values.value.tuple.values vs2", vs2)
            `_p` ("T M tuple.values.value.tuple.values vs", vs)
            `_f` ("T M value.tuple.values ts",ts)

    where

    handleNested ::  [TagT] -> E -> E
    handleNested ts1 e@[xMatch| _ := value.tuple.values |] =

        let ts2  = TagSingle "matrix" : ts1
            rs  = toEssenceRep ts2 e
            rs2 = after ts2 rs

        in rs2
        --in errb [rs2]

            --`_p` (" N T handleNerted res2",[rs2])
            `_f` (" ,N T handleNested ts",ts)
            `_p` (" ,N T handleNerted res",[rs])
            --`_p` (" ,N T handleNerted e2", [e2])
            `_p` (" ,N T handleNerted e",[e])
            `_f` (" ,N T handleNested ts",ts)

    handleNested _ f = f
    --handleNested ts f = f   `_p` (" N T handleNerted unchanged",[f])

    after :: [TagT] -> E -> E
    after ts2 [xMatch| vs2 := value.matrix.values |] =
        let vss =  map (toEssenceRep ts2) vs2
        in  [xMake| value.matrix.values := vss |]

    after _ f = f


-- CHECK add more guards
-- Check one two many matrices?
-- for mutiMatixMatixTupleComplex3Simpler2 (main' o7)
toEssenceRep r@[TagSingle "matrix", TagTuple ts ]
               [xMatch| vs := values
                      | _  := values.value.tuple.values|] =
    let res = map (toEssenceRep r) vs
    in  [xMake| values := res |]
        `_p` ("V M T values.value.tuple.values res", res)
        `_p` ("V M T values.value.tuple.values ts vs", vs)
        `_f` ("V M T values.value.tuple.values ts ", ts)


toEssenceRep r@(TagSingle t :ts)  [xMatch| [vals] := value.matrix |] |
    t /= "int" =
    let vals' = toEssenceRep ts vals in
        [xMake| value := [Tagged t [vals'] ] |]
        `_p` ("S value.matrix v vals'", unwrapValues vals')
        `_p` ("S value.matrix v args",  unwrapValues vals)
        `_f` ("S value.matrix ts", ts)
        `_f` ("S value.matrix tags", r)

-- FIXME separate relation and partition from tagsingle
toEssenceRep r@(TagSingle t :ts) [xMatch| arr := values.value|]  |
    t /= "int" =
    let vals' = map er arr
    in  [xMake| values := vals'|]
        `_p` ("S values.value res",vals')
        `_p` ("S values.value args", arr)
        `_f` ("S values.value ts",r)

    where
        er (Tagged "matrix" [vals]) = Tagged "value" [Tagged t  [toEssenceRep ts vals]]
        er a@(Tagged "literal" _)   = Tagged "value" [a]
        er a@[xMatch| _ := value.literal|] = a
        {-er f = f -}
        er f = upBug "AddEssenceTypes:er error" [f]

-- FIXME This really should not be needed
toEssenceRep r@[TagTuple _]  [xMatch| vals := expr.value.tuple |] =
    let res = toEssenceRep r (Tagged "tuple" vals )
    in [xMake| expr.value := [res] |]
        `_p` ("T expr.value.tuple res",[Tagged "value" [res]])
        `_p` ("T expr.value.tuple args", [Tagged "value" [Tagged "tuple" vals]])
        `_f` ("T expr.value.tuple ts",r)

toEssenceRep r@(TagTuple t : [])  [xMatch| vals := tuple.values |] =
    let zipped = (zip t vals)
        vals' = map func zipped
    in [xMake| tuple :=  [foldl1 combineValues vals'] |]
        `_e` ("T tuple.values res'",  vals')
        {-`_f` ("T tuple.values zipped", zipped)-}
        `_p` ("T tuple.values vals", vals)
        `_f` ("T tuple.values ts", r)
   where func (ts,val) =
            toEssenceRep ts (wrapper val)

         wrapper e@[xMatch| _ := values |]  = e
         wrapper e@[xMatch| _ := value  |]  = Tagged "values" [e]  -- wat?
         wrapper f = upBug "toEssenceRep wrapper " [f]

         combineValues :: E -> E  -> E
         combineValues [xMatch| v1 := values |]  v2@[xMatch| _:= value |] = [xMake| values := v1 ++ [v2] |]

         combineValues [xMatch| v1 := values |]  [xMatch| v2 := values |] = [xMake| values := v1 ++ v2 |]
         combineValues v1@[xMatch| _:= value |]  v2@[xMatch| _:= value |] = [xMake| values := [v1,v2] |]
         combineValues v1@[xMatch| _ := value |] [xMatch| v2:= values  |] = [xMake| values := v1 : v2 |]

toEssenceRep r@(TagTuple _ : [])  e@[xMatch| vals := values.value.tuple |] =
    let res = map func  vals
    in [xMake| values := res |]
        `_p` ("T values.value.tuple res",res)
        `_p` ("T values.value.tuple vals", unwrapValues (head vals) )
        `_p` ("T values.value.tuple args", unwrapValues e)
        `_f` ("T values.value.tuple", r)
    where func val =
           Tagged "value" [
                toEssenceRep r (Tagged "tuple" [val])
           ]

-- convert matrices rep to tuples
toEssenceRep r@(TagTuple t : []) [xMatch| vals  := values.value.matrix |] =
    let zipped= map ( zip t . unwrapValues ) vals
        vals' =  map convert zipped
        vv = map (\v ->  [xMake| value.tuple := [v] |] ) vals
        vmap = map (\(Tagged "value" [v]) -> Tagged "value" [toEssenceRep r v]) vv

    in  [xMake| values := vmap |]
         `_p` ("T values.value.matrix",vmap)
         `_p` ("T values.value.matrix vv", vv)
         `_i` ("T values.value.matrix (r,val')", (r,vals'))
         `_f` ("T values.value.matrix zipped", zipped)
         `_e` ("T values.value.matrix args", vals)

    where wrap _v = [xMake| value.tuple := [_v] |]
          sub :: ([TagT],E) -> [E]
          sub (ts,val) = unwrapValues $ toEssenceRep ts (Tagged "values" [val])

          convert zipped  = wrap $ Tagged "values" $  concatMap sub zipped

toEssenceRep r@[TagTuple _] e@[xMatch| _  := values.value
                                     | vs := values|] =
    let res =   map (\v -> toEssenceRep r [xMake| tuple.values := [v] |]  ) vs
        res' =  map (\v -> [xMake| value := [v] |]) res
    in  [xMake| values := res' |]

    `_e` ("T values.value res'",  res')
    `_e` ("T values.value res",  res)
    `_e` ("T values.value vs",  vs)
    `_e` ("T values.value args", [e])
    `_f` ("T values.value ts", r)

-- fixes mutiMatixMatixTupleComplex3Simpler5 (o85) and mutiMatixMatixTupleComplex3Simpler4
toEssenceRep r@[TagTuple ts] e@[xMatch| vs  := value.tuple.values |] =
    let res  = zipWith (\t f -> toEssenceRep (TagSingle "matrix": t ) f )  ts vs
        res' = [xMake| value.tuple.values := res |]
    in res'
    `_p` ("T value.tuples.value res'",  [res'])
    `_p` ("T value.tuples.value res",  res)
    `_p` ("T value.tuples.value vs",  vs)
    `_p` ("T value.tuples.value args", [e])
    `_f` ("T value.tuples.value ts", r)


toEssenceRep r@[TagFunc ins tos] [xMatch| arr := value.function.values |] =
    let mappings =  map (func ins tos) arr
    in  [xMake| value.function.values := mappings |]

    `_p` ("F mapping", arr)
    `_f` ("F ts", r)

    where
    func ins' tos' [xMatch| [a,b] := mapping |] =
       let a' = unwrapExpr $ toEssenceRep ins'  (wrapInExpr [a])
           b' = unwrapExpr $ toEssenceRep tos'  (wrapInExpr [b])
       in   [xMake| mapping := [a',b'] |]
    func _ _ _  = bug "AddEssenceTypes: toEssenceRep function error"



toEssenceRep [TagSingle "int"] e@[xMatch|  [Prim (I _)] := value.literal |] = e
    `_p` ("toEssenceRep Int ", [e])


toEssenceRep r e@[xMatch| [val] := expr |] =
    let res  = toEssenceRep r val
        res2 = flattenInt r res
    in [xMake| expr := [res2] |]
        `_k` ("expr", (r, [e]) )

toEssenceRep r e =
    e
    `_p` ("toEssenceRep no match vs",  [e])
    `_f` ("toEssenceRep no match ts", r)


-- FIXME  should not be needed
-- flattens [[1]]  when it is ment to be just 1
flattenInt :: [TagT] -> E -> E
flattenInt [TagSingle "int"]  e@[xMatch| [ele] := value.matrix.values |]
    | isJust res = fromJust res
        `_p` ("flattenInt unwraping int res", [res])
        `_p` ("flattenInt unwraping int e", [e])
    where res = getLit ele

flattenInt  (TagSingle "matrix":ts) [xMatch| vs := value.matrix.values |] =
    let res = map (flattenInt ts) vs
    in  [xMake| value.matrix.values := res |]

flattenInt  [TagTuple ts] [xMatch| vs := value.tuple.values |] =
    let res = zipWith flattenInt ts vs
    in  [xMake| value.tuple.values := res |]

flattenInt _  e = e
