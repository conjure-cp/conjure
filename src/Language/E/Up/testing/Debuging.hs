{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Language.E.Up.Testing.Debugging where

import Language.E hiding(trace)
import Language.E.Pipeline.InlineLettings(inlineLettings)

import Language.E.Up.EprimeToEssence
import Language.E.Up.IO
import System.Process (runCommand)
import System.FilePath

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

import Control.Arrow(arr,(&&&),first, second)


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
    org <- getSpec' False orgF
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

base     =  "/Users/bilalh/CS/conjure/files/upTests/"
getTest' = getFiles base
getTest  = flip (getFiles base) 1
gg n s' = let s = dropExtension s' in getFiles base  (fromMaybe s (stripPrefix base s) ) n

bg n s = be (gg n s)
be specs@(specF, _, orgF ,Just paramF,Just orgParamF) = do
    (spec,sol,org,orgP) <- getTestSpecs specs
    putStrLn "Solution"
    s specs
    putStrLn ""

    let logsF = addExtension specF "logs"
    logs <- liftM T.lines (T.readFile logsF)
    let tuplesOfMatrixes =  makeTuplesOfMatrixesMap logs
    __groomPrintM "tuplesOfMatrixes" tuplesOfMatrixes
    putStrLn ""

    let enumMapping1 = getEnumMapping org
        enums1       = getEnumsAndUnamed org

        (enumMapping, enums) = convertUnamed enumMapping1 enums1
    __groomPrintM "enum mapping" enumMapping

    let varInfo1 = getVariables spec
        orgInfo  = getEssenceVariables enumMapping org
        solInfo1 = getSolVariables sol
        -- I don't think I need the aux variables
        solInfo2 = M.filterWithKey (\a _ ->  not $ isPrefixOf "v__" a) solInfo1
        solInfo  = M.filterWithKey (\a _ ->  not $ isPrefixOf "aux__" a) solInfo2
        varInfo2 = M.filterWithKey (\a _ ->  not $ isPrefixOf "v__" a) varInfo1
        varInfo  = M.filterWithKey (\a _ ->  not $ isPrefixOf "aux__" a) varInfo2

    let varTrees = createVarTree varInfo
        varMap1  = combineInfos varInfo solInfo
        varMap   = M.map (\vd@VarData{vEssence=e} -> vd{vEssence=unwrapExpr e} ) varMap1

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


    let indexrangeMapping = gatherIndexRanges (inlineSpec org)
    __groomPrintM "indexrangeMapping" indexrangeMapping

    let lookUp m k= fromMaybe (error $ "fromMaybe cgs: lookUpType: " ++ (show k) ++ " " ++ (show m) )  . flip M.lookup m $ k
        eval (s,e) =
            let orgType     = lookUp orgInfo s
                indext      = lookUp indexrangeMapping s
                res'        = introduceTypes enumMapping orgType e
                withIndexes = introduceIndexRange indext res'
            in wrap s $ withIndexes

        resultEssence =  map eval varResults
        {-resultEssence =  map (uncurry wrap ) varResults-}

    (print . vcat . map pretty) resultEssence
    {-mapM_ (print . prettyAsPaths) resultEssence-}
    putStrLn "[Finished]"

