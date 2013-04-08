{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Language.E.Up.Testing.Debugging where


import Language.E
import Language.E.Up.EprimeToEssence
import Language.E.Up.IO
import System.Process (runCommand)
import System.FilePath

import Language.E.Up.ReduceSpec
import Language.E.Up.GatherInfomation
import Language.E.Up.RepresentationTree
import Language.E.Up.EvaluateTree
import Language.E.Up.AddEssenceTypes
import Language.E.Up.Debug
import Language.E.Up.Data
import Language.E.Up.Common
import Language.E.Up.GatherIndexRanges

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
    (putStrLn . groom) es


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


-- Print out using eval
be specs@(_, _, orgF ,_ ,_) = do
    (spec,sol,org,_) <- getTestSpecs specs

    let varInfo1 = getVariables spec
        orgInfo  = getEssenceVariables org
        solInfo1 = getSolVariables sol
        -- I don't think I need the aux variables
        solInfo2 = M.filterWithKey (\a _ ->  not $ isPrefixOf "v__" a) solInfo1
        solInfo  = M.filterWithKey (\a _ ->  not $ isPrefixOf "aux__" a) solInfo2
        varInfo2 = M.filterWithKey (\a _ ->  not $ isPrefixOf "v__" a) varInfo1
        varInfo  = M.filterWithKey (\a _ ->  not $ isPrefixOf "aux__" a) varInfo2

    let varTrees = createVarTree varInfo
        varsData = combineInfos varInfo solInfo

    --__groomPrintM "org" org
    __groomPrintM "orgInfo" orgInfo
    __groomPrintM "varInfo" varInfo
    {-__groomPrintM "solInfo" solInfo-}
    __groomPrintM "varTrees" varTrees
    --__groomPrintM "varsData" varsData

    let varResults = map (\t -> evalTree (onlyNeeded varsData t ) t ) varTrees

    let
        wrap :: String -> E -> E
        wrap name (Tagged "expr" arr) =
            [xMake| topLevel.letting.expr := arr
                  | topLevel.letting.name.reference := [Prim (S (T.pack name))] |]
        wrap n e = errbM "cgs: wrap failed"  [e]


    let resultEssence =  map (uncurry wrap ) varResults
    (print . vcat . map pretty) resultEssence

c = 1


logFile = "/Users/bilalh/CS/conjure/files/upTests/_zothers/tupley27-1-1m/0001.eprime.logs"

_rm = do
    text <- T.readFile logFile
    return $ makeTuplesOfMatrixesSet (T.lines text)


makeTuplesOfMatrixesSet :: [T.Text] -> Set String 
makeTuplesOfMatrixesSet = 
      S.fromList
    . map (dropWhile isSpace . T.unpack)
    . nub
    . mapMaybe (T.stripPrefix "[matrixToTuple]")

-- Stages of tupley27-1-1m 
test  = [eMake| ([4,5], [8,9]) |]
test2 = [eMake| ( [1,2], [(4, 8), (5, 9)]) |]
test3 = [eMake| tuple( [(1, (4, 8)), (2, (5, 9))] ) |]
--   [tuple ((1, (4, 8))), tuple ((2, (5, 9)))]

reverseTuplesOfMatrixes ::  E -> E
reverseTuplesOfMatrixes [xMatch| vs := value.tuple.values |] =
    wrapInMatrix . map matrixToTuple $ transposeE vs

reverseTuplesOfMatrixes e = bug $ "reverseTuplesOfMatrixes called on " <+> pretty e


-- impure main for printing stuff
main' = mainn True
mainf = mainn False
mainn bool specs@(specF, _, _ ,_ ,_) = do
    (spec,sol,org,orgP) <- getTestSpecs specs

    let logsF = replaceExtension specF "logs"
    logs <- liftM T.lines (T.readFile logsF)
    let tuplesOfMatrixes =  makeTuplesOfMatrixesSet logs

    (print . pretty) orgP
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
        varsData = combineInfos varInfo solInfo

    --__groomPrintM "org" org
    --__groomPrintM "spec" spec
    __groomPrintM "orgInfo" orgInfo
    __groomPrintM "varInfo" varInfo
    --__groomPrintM "solInfo" solInfo
    __groomPrintM "varTrees" varTrees
    --__groomPrintM "varsData" varsData


    let varResults = map (\t -> evalTree (onlyNeeded varsData t ) t ) varTrees
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

#ifdef UP_DEBUG
n1 = getTest "_zothers/tupley27"
n2 = getTest "_zothers/tupley27-1-1m"

p0 = getTest "___types/partition"

f1  = getTest  "_functions/setOfFunc2"
f2  = getTest  "_functions/funcSet"
f3  = getTest  "_functions/funcSetNested"
f4  = getTest  "_functions/setOfFuncSetNested"
f5  = getTest  "_functions/setOfFuncTupleSet"
f51 = getTest  "_functions/funcTupleSet"
f6  = getTest  "_functions/setOfFuncTupleSetMatrix"
f7  = getTest  "_functions/func_setToInts"
f8  = getTest  "_functions/func_msetToInts"
f9  = getTest  "_functions/setOfFuncTupleSetFromSet"
f10 = getTest  "_functions/func_intToTuple"
f11 = getTest  "_functions/func_setToTuple"
f12 = getTest  "_functions/simpleMultiFunc"

c2 = getTest "_zzComplex/tupley32-8m"
c3 = getTest "_zzComplex/tupley32-8-t2m"
c4 = getTest "_zzComplex/tupley32-8Complex3"
c5 = getTest "_zzComplex/tupley32-8Complex4"
c6 = getTest "_zzComplex/tupley32-8Complex5"

md3 = getTest "_muti_dimensional/tupley15/3d_tupley15"
md4 = getTest "_muti_dimensional/tupley15/4d_tupley15"
md5 = getTest "_muti_dimensional/tupley15/5d_tupley15"
mdm = getTest "_muti_dimensional/withMutiMutiMatrix3m/2d_withMutiMutiMatrix3m"

i120m = getTest "__issues/120-funcToSet-matrix1d"
rei =  getTest' "__reps/RelationIntMatrix2/set_of_first" 2


ind = getTest "_indexes/2dMatrix"
#endif
