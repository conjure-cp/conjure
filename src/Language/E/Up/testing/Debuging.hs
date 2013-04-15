{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Language.E.Up.Testing.Debugging where


import Language.E hiding(trace)

import Language.E.Up.EprimeToEssence
import Language.E.Up.IO
import System.Process (runCommand)
import System.FilePath

import Language.E.Up.AddEssenceTypes
import Language.E.Up.Common
import Language.E.Up.Data
import Language.E.Up.Debug
import Language.E.Up.EvaluateTree2
import Language.E.Up.GatherIndexRanges
import Language.E.Up.GatherInfomation
import Language.E.Up.ReduceSpec
import Language.E.Up.RepresentationTree
import Language.E.Up.Representations

import Data.List(stripPrefix)
import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char(isSpace)


                            {-              -}
                            -- For Debuging --
                            {-              -}

gir specs@(specF, solF, orgF,param,orgParam) =do
    (_,_,_,ess@(Spec _ es)) <- getTestSpecs specs
    (Spec _ esol) <- getSpec solF
    mapM_ (print . prettyAsBoth) (statementAsList esol)
    putStrLn ""
    mapM_ (print . prettyAsBoth) (statementAsList es)
    putStrLn ""    
    let info  = gatherIndexRanges ess
    putStrLn . groom $ info


n specs@(specF, solF, orgF,param,orgParam) =
  putStrLn (takeFileName orgF)

s specs@(specF, solF, orgF,param,orgParam) = do
    let esolF = addExtension (dropExtensions solF) "solution"
    (Spec _ es) <- getSpec esolF
    (print . pretty) es
    {-(putStrLn . groom) es-}


-- Reveal files
sol specs@(specF, solF, orgF,param,orgParam) = do
    let esolF = addExtension (dropExtensions solF) "solution"
    runCommand $ "open -R \"" ++ esolF ++ "\""

ess = org
o   = org
org specs@(specF, solF, orgF,param,orgParam) =
    runCommand $ "open -R \"" ++ orgF ++ "\""

m specs@(specF, solF, orgF,param,orgParam) =
    runCommand $ "mvim \"" ++ orgF ++ "\""

po specs@(specF, solF, orgF,param,orgParam)  = do
    org <- getSpec orgF
    mapM_ __groomPrint    (toLst org)
    --(print . pretty) org
    return ()

ps specs@(specF, solF, orgF,param,orgParam)  = do
    (spec,sol,org,_) <- getTestSpecs specs
    {-mapM_ __groomPrint  (toLst sol)-}
    getSpec solF >>=  putStrLn . groom
    return ()

e= es
es specs@(specF, solF, orgF,param,orgParam)  = do
    (spec,sol,s@(Spec  _ es),_) <- getTestSpecs specs
    (print . pretty) s
    return ()

t specs@(specF, solF, orgF,param,orgParam)  = do
    (spec,sol,org,_) <- getTestSpecs specs
    let orgInfo  = getEssenceVariables org
    return orgInfo

ty a = t a >>= __groomPrint

ep specs@(specF, solF, orgF,param,orgParam)  = do
    (spec,sol,org,_) <- getTestSpecs specs
    (print . pretty) spec
    let varInfo1 = getVariables spec
    return varInfo1


rs specs@(specF, solF, orgF,param,orgParam)  = do
    sol <- getSpec solF
    {-(print . pretty) sol-}
    (Spec  _ es) <- getSpec orgF >>= reduceSpec
    (print . pretty) es

bes specs=  do
    be specs
    putStrLn "solution"
    s specs

-- Print out using eval
be specs@(specF, _, orgF ,_ ,_) = do
    (spec,sol,org,orgP) <- getTestSpecs specs
    (print . pretty) orgP


    let logsF = addExtension specF "logs"
    logs <- liftM T.lines (T.readFile logsF)
    let tuplesOfMatrixes =  makeTuplesOfMatrixesSet logs
    __groomPrintM "tuplesOfMatrixes" tuplesOfMatrixes
    putStrLn ""

    let varInfo1 = getVariables spec
        orgInfo  = getEssenceVariables org
        solInfo1 = getSolVariables sol
        -- I don't think I need the aux variables
        solInfo2 = M.filterWithKey (\a _ ->  not $ isPrefixOf "v__" a) solInfo1
        solInfo  = M.filterWithKey (\a _ ->  not $ isPrefixOf "aux__" a) solInfo2
        varInfo2 = M.filterWithKey (\a _ ->  not $ isPrefixOf "v__" a) varInfo1
        varInfo  = M.filterWithKey (\a _ ->  not $ isPrefixOf "aux__" a) varInfo2

    let varTrees = createVarTree varInfo
        varMap1  = combineInfos varInfo solInfo
        varMap   = M.map (\vd@VarData{vEssence=e} -> vd{vEssence=unwrapExpr e} ) varMap1

    --__groomPrintM "org" org
    __groomPrintM "orgInfo" orgInfo
    __groomPrintM "varInfo" varInfo
    {-__groomPrintM "solInfo" solInfo-}
    __groomPrintM "varTrees" varTrees
    --__groomPrintM "varMap" varMap

    let varResults = map (\t -> evalTree (onlyNeeded varMap t ) tuplesOfMatrixes t ) varTrees
        
    let
        wrap :: String -> E -> E
        wrap name value =
            [xMake| topLevel.letting.expr := [value]
                  | topLevel.letting.name.reference := [Prim (S (T.pack name))] |]


    let indexrangeMapping = gatherIndexRanges orgP    

    let enumMapping1 = getEnumMapping orgP
        enums1       = getEnumsAndUnamed orgP

        (enumMapping, enums) = convertUnamed enumMapping1 enums1

    let lookUp m = fromMaybe (error "fromMaybe cgs: lookUpType")  . flip M.lookup m
        eval (s,e) =
            let orgType = lookUp orgInfo s
                indext  = lookUp indexrangeMapping s
                res'    = introduceTypes enumMapping orgType e
                withIndexes =   introduceIndexRange indext res'
            in wrap s $ res' 

        resultEssence =  map eval varResults
        {-resultEssence =  map (uncurry wrap ) varResults-}

    (print . vcat . map pretty) resultEssence
    {-mapM_ (print . prettyAsPaths) resultEssence-}




logFile = "/Users/bilalh/CS/conjure/files/upTests/_zothers/tupley27-1-1m/0001.eprime.logs"

_rm = do
    text <- T.readFile logFile
    return $ makeTuplesOfMatrixesSet (T.lines text)


-- Stages of tupley27-1-1m 
test  = [eMake| ([4,5], [8,9]) |]
test2 = [eMake| ( [1,2], [(4, 8), (5, 9)]) |]
test3 = [eMake| tuple( [(1, (4, 8)), (2, (5, 9))] ) |]
--   [tuple ((1, (4, 8))), tuple ((2, (5, 9)))]

reverseTuplesOfMatrixes ::  E -> E
reverseTuplesOfMatrixes [xMatch| vs := value.tuple.values |] =
    wrapInMatrix . map matrixToTuple $ transposeE vs

reverseTuplesOfMatrixes e = bug $ "reverseTuplesOfMatrixes called on " <+> pretty e



run :: [(Before, After)] -> VarData -> VarData
run  fs starting   = evalFs fs id starting
run' fs starting f = evalFs fs (liftRep f) starting

evalFs :: [(Before, After)] -> BranchFunc -> VarData -> VarData
evalFs []     mid v =  v
evalFs [g]    mid v =  f g mid v
evalFs (g:gs) mid v =  f g (evalFs gs mid ) v 

-- Run the before transformtion the inner function then the after transformtion
f :: (Before,After) -> BranchFunc -> VarData -> VarData
f (before,after) mid value = 
    let vs     = tracer "before:" $ before  (tracer "value:" value)
        mids   = tracer "mid:" $ map mid vs
        res    = tracer "after:" $ after value mids
    in res

unwrapSet :: VarData -> [VarData]
unwrapSet v@VarData{vEssence=e, vIndexes=ix} =  
        map (\f -> v{vEssence=f, vIndexes=tail ix} )  (unwrapMatrix e)

mapLeafFunc :: (VarData -> E) -> VarData -> [VarData] -> VarData
mapLeafFunc f orgData vs = 
        orgData{vEssence = wrapInMatrix . map f $  vs }

mapLeafUnchanged :: VarData -> [VarData] -> VarData
mapLeafUnchanged = mapLeafFunc vEssence 

explicit :: (Before,After)
explicit = ( unwrapSet, mapLeafUnchanged )

occurrence :: (Before,After)
occurrence = ( unwrapSet, mapLeafFunc setOccurrenceRep )

matrix1D :: (Before,After)
matrix1D = ( unwrapSet, mapLeafFunc matrix1DRep )


partitionMSetOfSets :: (Before,After)
partitionMSetOfSets = ( before, after )
    where 
    before v = [v]
    after orgData [vs] = vs{vEssence=partitionMSetOfSetsRep vs}

liftRep ::  (VarData -> E) -> VarData  -> VarData
liftRep repFunc vdata  = vdata{vEssence=repFunc vdata} 


vTest = VarData{
        vIndexes = [[1, 2, 3], [0, 1, 2]]
        ,vBounds = [0, 1]
        ,vEssence = [eMake| [ [false,false,true], [false,true,true], [true,true,true]] |]
        }

vFuncSetnR = run' [matrix1D,explicit] vFuncSetn setOccurrenceRep
vFuncSetn = VarData{vIndexes = [[2, 3, 4], [1, 2], [5, 6, 7, 8]],
        vBounds = [0, 1],
        vEssence =[eMake| 
        [[[false, true, true, true],
          [true, false, true, true]],
         [[false, true, true, true],
          [true, false, true, true]],
         [[false, true, true, true],
          [true, false, true, true]]]
        |]}

vFuncSetR = run' [matrix1D] vFuncSet setOccurrenceRep
vFuncSet = VarData {vIndexes = [[2, 3, 4], [5, 6]]
         ,vBounds = [0, 1]
         ,vEssence =
             [eMake| [[true, true], [true, true], [true, true]]
             |]}

vParF = run' [partitionMSetOfSets, explicit] vPar setOccurrenceRep
vPar = VarData {vIndexes = [[1, 2, 3], [1, 2, 3, 4, 5, 6, 7, 8, 9]]
         ,vBounds = [0, 1]
         ,vEssence = [eMake|
         [[false, false, false, false, false, false, false, false, true],
          [false, false, false, false, true, true, true, true, false],
          [true, true, true, true, false, false, false, false, false]]
          |]}


c = 1
-- impure main for printing stuff
main' = mainn True
mainf = mainn False
mainn bool specs@(specF, _, _ ,_ ,_) = do
    (spec,sol,org,orgP) <- getTestSpecs specs

    (print . pretty) orgP

    let logsF = addExtension specF "logs"
    logs <- liftM T.lines (T.readFile logsF)
    let tuplesOfMatrixes =  makeTuplesOfMatrixesSet logs
    __groomPrintM "tuplesOfMatrixes" tuplesOfMatrixes
    


    let varInfo1 = getVariables spec
        orgInfo  = getEssenceVariables org
        solInfo1 = getSolVariables sol
        -- I don't  need the aux variables
        solInfo2 = M.filterWithKey (\a _ ->  not $ isPrefixOf "v__" a) solInfo1
        solInfo  = M.filterWithKey (\a _ ->  not $ isPrefixOf "aux__" a) solInfo2
        varInfo2 = M.filterWithKey (\a _ ->  not $ isPrefixOf "v__" a) varInfo1
        varInfo  = M.filterWithKey (\a _ ->  not $ isPrefixOf "aux__" a) varInfo2

    let enumMapping1 = getEnumMapping orgP
        enums1       = getEnumsAndUnamed orgP

        (enumMapping, enums) = convertUnamed enumMapping1 enums1

    {-__groomPrintM "enumMapping1" enumMapping1-}
    {-__groomPrintM "enums1" enums1-}
    __groomPrintM "enumMapping" enumMapping
    --putStrLn "enums:"
    --mapM (print . pretty) enums

    let indexrangeMapping = gatherIndexRanges orgP

    let varTrees = createVarTree varInfo
        varMap1  = combineInfos varInfo solInfo
        varMap   = M.map (\vd@VarData{vEssence=e} -> vd{vEssence=unwrapExpr e} ) varMap1

    --__groomPrintM "org" org
    --__groomPrintM "spec" spec
    __groomPrintM "orgInfo" orgInfo
    __groomPrintM "varInfo" varInfo
    --__groomPrintM "solInfo" solInfo
    __groomPrintM "varTrees" varTrees
    --__groomPrintM "varsData" varsData


    let varResults = map (\t -> evalTree (onlyNeeded varMap t ) tuplesOfMatrixes t ) varTrees
    --__groomPrintM "varResults" varResults

    -- Wrap all the variables with the required essence stuff
    let
        wrap :: String -> E -> E
        wrap name (Tagged "expr" arr) =
            [xMake| topLevel.letting.expr := arr
                  | topLevel.letting.name.reference := [Prim (S (T.pack name))] |]
        wrap n e = errbM "cgs: wrap failed"  [e]

    let lookUp m = fromMaybe (error "fromMaybe cgs: lookUpType")  . flip M.lookup m
        eval (s,e) =
            let orgType = lookUp orgInfo s
                indext = lookUp indexrangeMapping s
                (changed, _type) = convertRep orgType
                res  = if bool then iterate (toEssenceRep _type) e !! c else toEssenceRep' _type e
                res' = introduceTypes enumMapping orgType res
            in wrap s $ introduceIndexRange indext (if changed then res' else res)

        resultEssence =  map eval varResults
        --resultEssence =  map (uncurry wrap ) varResults


    putStrLn . groom $ indexrangeMapping
    putStrLn ""
    --__groomPrintM "resultEssence" resultEssence
    (print . vcat . map pretty) (enums ++  resultEssence)
    --mapM_ (print . prettyAsTree) (enums ++  resultEssence)
    --return resultEssence

base     =  "/Users/bilalh/CS/conjure/files/upTests/"
getTest' = getFiles base
getTest  = flip (getFiles base) 1


gg n s' = let s = dropExtension s' in getFiles base  (fromMaybe s (stripPrefix base s) ) n

func1d     = getTest  "___types/func_matrix1d"
func1dSetn = getTest  "_functions/funcSetNested"
func1dSet  = getTest  "_functions/funcSet"

par = getTest "_partition/partition_lit"

sets = getTest' "__simple-sets/sets"


#ifdef UP_DEBUG
n1 = getTest "_zothers/tupley27"
n2 = getTest "_zothers/tupley27-1-1m"

f1  = getTest  "_functions/setOfFunc2"
f2  = getTest  "_functions/funcSet"
f3  = getTest  "_functions/funcSetNested"
f4  = getTest  "_functions/setOfFuncSetNested"
f5  = getTest' "_functions/setOfFuncTupleSet" 2      -- TODO
f51 = getTest  "_functions/funcTupleSet"             -- TODO
f6  = getTest  "_functions/setOfFuncTupleSetMatrix"  -- TODO
f7  = getTest  "_functions/func_setToInts"           -- TODO
f8  = getTest  "_functions/func_msetToInts"          -- TODO
f9  = getTest  "_functions/setOfFuncTupleSetFromSet" -- TODO
f10 = getTest' "_functions/func_intToTuple" 2        
f11 = getTest  "_functions/func_setToTuple"          -- TODO
f12 = getTest  "_functions/simpleMultiFunc"          -- TODO

c2 = getTest "_zzComplex/tupley32-8m"
c3 = getTest "_zzComplex/tupley32-8-t2m"
c4 = getTest "_zzComplex/tupley32-8Complex3"
c5 = getTest "_zzComplex/tupley32-8Complex4"
c6 = getTest "_zzComplex/tupley32-8Complex5"

md3 = getTest "_muti_dimensional/tupley15/3d_tupley15"
md4 = getTest "_muti_dimensional/tupley15/4d_tupley15"
md5 = getTest "_muti_dimensional/tupley15/5d_tupley15"
mdm = getTest "_muti_dimensional/withMutiMutiMatrix3m/2d_withMutiMutiMatrix3m"

--TODO
i120m = getTest "__issues/120-funcToSet-matrix1d"

rei =  getTest' "__reps/RelationIntMatrix2/set_of_first" 2


ind = getTest "_indexes/2dMatrix"
#endif


