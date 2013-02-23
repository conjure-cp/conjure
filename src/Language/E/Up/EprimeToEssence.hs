{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
module Language.E.Up.EprimeToEssence where

import Language.E

import Language.E.Up.Data
import Language.E.Up.GatherInfomation
import Language.E.Up.RepresentationTree
import Language.E.Up.EvaluateTree
import Language.E.Up.AddEssenceTypes

import qualified Data.Text as T
import qualified Data.Map as M

mainPure :: (Spec, Spec, Spec, Spec) -> [E]
mainPure (spec,sol,org,unalteredOrg) =
    let varInfo1 = getVariables spec
        orgInfo  = getEssenceVariables org
        solInfo1 = getSolVariables sol
        -- I don't think I need the aux variables
        solInfo  = M.filterWithKey (\a _ ->  not $ isPrefixOf "v__" a) solInfo1
        varInfo  = M.filterWithKey (\a _ ->  not $ isPrefixOf "v__" a) varInfo1

        enumMapping1 = getEnumMapping unalteredOrg
        enums1       = getEnumsAndUnamed unalteredOrg

        (enumMapping, enums) = convertUnamed enumMapping1 enums1

        varTrees = createVarTree varInfo
        varsData = combineInfos varInfo solInfo

        varResults = map (evalTree varsData) varTrees

        wrap :: String -> E -> E
        wrap name (Tagged "expr" arr) =
            [xMake| topLevel.letting.expr := arr
                  | topLevel.letting.name.reference := [Prim (S (T.pack name))] |]

        lookUpType = fromMaybe (error "fromMaybe eTe: lookUpType")  . flip M.lookup orgInfo
        eval (s,e) = 
            let orgType = lookUpType s
                (changed, _type) = convertRep orgType
                res  = toEssenceRep _type e
                res' = introduceTypes enumMapping orgType res
            in wrap s (if changed then res' else res)

        resultEssence   = map eval varResults
        --resultEssence =  map (uncurry wrap) varResults

    in enums ++ resultEssence


combineInfos ::  M.Map String VarInfo -> M.Map String [E] -> M.Map String VarData
combineInfos varInfo solInfo =
   combineInfo (M.toAscList varInfo) (M.elems solInfo)

    where

    combineInfo :: [(String,VarInfo)] ->  [[E]] -> M.Map String VarData
    combineInfo  vs es = M.fromList $  zipWith combine' vs es

    combine' :: (String,VarInfo) -> [E] -> (String,VarData)
    combine' (s, VarInfo{indexes = ix, bounds = b}) es =
        (s,VarData{vIndexes=ix, vBounds=b, vEssence= es !! 1})


convertRep :: [TagT] -> (Bool,[TagT])
convertRep arr = 
  let (bs,res) =  unzip $ map convertRep' arr
  in  (or bs, res)

convertRep' :: TagT -> (Bool,TagT)
convertRep' (TagSingle t) =  (b', TagSingle res)
    where (b',res) = convertTag t

convertRep' (TagEnum _  ) =  (True, TagSingle "int")
convertRep' (TagUnamed _) =  (True, TagSingle "int")

convertRep' (TagTuple ts) = (or b', TagTuple res)
    where (b',res) =  unzip $ map convertRep ts

convertRep' (TagFunc ins tos) = ( b1 || b2, TagFunc ins' tos')
    where 
        (b1,ins') =  convertRep ins
        (b2,tos') =  convertRep tos 

convertTag :: Tag -> (Bool,Tag)
convertTag "set"  = (True, "matrix")
convertTag "mset" = (True, "matrix")
convertTag "bool" = (True, "int")
convertTag t      = (False, t)

introduceTypes ::  M.Map String [E] -> [TagT] -> E -> E
introduceTypes emap ts [xMatch| [val] := expr |] = 
    let res = introduceTypes emap ts val
    in  [xMake| expr := [res] |]

introduceTypes emap [TagEnum name] [xMatch| [Prim (I num)] := value.literal |] = 
    let res = fromMaybe (error "fromMaybe enums") $ M.lookup name emap
        selected = res !! fromInteger (num - 1)
    in  selected 

introduceTypes emap [TagUnamed kind] e@[xMatch| [Prim (I _)] := value.literal |] = 
    introduceTypes emap [TagEnum ("__named_" ++ kind)] e

introduceTypes _ [TagSingle "bool"] [xMatch| [Prim (I num)] := value.literal |] = 
    let bool = num == 1
    in  [xMake| value.literal := [Prim (B bool)] |] 

introduceTypes emap (TagSingle "matrix":ts) [xMatch| vs := value.matrix.values |] = 
    let res = map (introduceTypes emap ts) vs
    in  [xMake| value.matrix.values := res |] 

introduceTypes emap (TagSingle "set":ts) [xMatch| vs := value.matrix.values |] = 
    let res = map (introduceTypes emap ts) vs
    in  [xMake| value.set.values := res |] 

introduceTypes emap (TagSingle "mset":ts) [xMatch| vs := value.matrix.values |] = 
    let res = map (introduceTypes emap ts) vs
    in  [xMake| value.mset.values := res |] 

introduceTypes emap [TagTuple ts] [xMatch| vs := value.tuple.values |] = 
    let res = zipWith (introduceTypes emap) ts vs
    in  [xMake| value.tuple.values := res |] 

-- TODO stuff inside a partition and functions

introduceTypes _ _ e = e
-- introduceTypes _ ts e = errr (ts,e)

convertUnamed :: M.Map String [E] -> [E]  -> (M.Map String [E], [E])
convertUnamed m arr =
    let enumsParts = mapMaybe convertU arr
        m'         = M.union m (M.fromList enumsParts)
        enums      = map toEnum' enumsParts
        arr'       = arr ++ enums
    in  (m', arr')

    where toEnum' (name,es) = 
            [xMake| topLevel.letting.name.reference  := [Prim (S ref) ]
                  | topLevel.letting.typeEnum.values := es |]

            where ref = T.pack  name


convertU :: E -> Maybe (String, [E])
convertU [xMatch| [Prim (S name)] := topLevel.letting.name.reference
                | [Prim (I num)]  := topLevel.letting.typeUnnamed.value.literal  |] =
    let vals = map (createEnum name ) [1..num]
    in Just ("__named_" ++ T.unpack name, vals)

    where
    createEnum :: Text -> Integer -> E
    createEnum name1 num1 = [xMake| reference := [Prim (S ref )] |]

        where ref = T.concat [ "_", name1, "_", (T.pack . show) num1]

convertU _ = Nothing

