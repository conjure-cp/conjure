{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}

module Language.E.Up.EvaluateTree2 (
     evalTree
     ,reverseTuplesOfMatrixes
) where

import Bug
import Language.E

import Language.E.Up.Common(transposeE,matrixToTuple,unwrapMatrix,wrapInMatrix)
import Language.E.Up.Data
import Language.E.Up.Debug
import Language.E.Up.Representations

import Data.Map(Map)
import qualified Data.Map as M

type VarMap             = Map String VarData
type IsTuplesOfMatrixes = Map [String] Int

evalTree ::VarMap -> IsTuplesOfMatrixes -> Tree String  -> (String,E)
evalTree mapping set (Branch name arr) =
    (name, evalTree' mapping set [] [name] (repSelector arr))
    `_g` ("evalTree mapping", mapping)
    `_g` ("evalTree name", name)

evalTree mapping set tree@(Leaf name) =
    (name,  evalTree' mapping set [] []  tree)

evalTree _ _ _ = _bugg "evalTree no match"


evalTree' :: VarMap -> IsTuplesOfMatrixes -> [RepName] -> [String] -> Tree String  -> E
evalTree' mapping _ fs prefix (Leaf part) =
   let leafFunc = leafRep part
         `_p` ("leaf \n" ++ name, [vdata] )
         `_g` ("leaf funcs str", fs )

       res     = runBranchFuncs (reverse . mapMaybe getBranch $ fs) vdata leafFunc
   in  vEssence res


   where
   name    = intercalate "_" (prefix ++ [part])
   lookUpE e = fromMaybe (_bugg $ "fromMaybe: lookUpE evalTree' - " ++ (show .pretty $ e) ++ " mapping: " ++ show mapping ) 
               . flip M.lookup mapping $ e
   vdata   = lookUpE  name

evalTree' mapping set fs prefix (Tuple arr) =
    let items =  map ( evalTree' mapping set (mapMaybe repsToPassToTuple fs) prefix ) arr
        tuple = [xMake| value.tuple.values := items |]
        res   = handleTuplesOfMatrixes tuple
        -- Have to apply the inner rep conversion function first
        afterFuncs = runBranchFuncs (reverse . mapMaybe getBranch $ fs ) res noRep
    in  vEssence afterFuncs

     `_p` ("tuple_afterFuncs",[afterFuncs] )
     `_p` ("tuple_fs str",fs )
     `_p` ("tuple_res",[res] )
     `_p` ("tuple+ans",[tuple] )
     `_p` ("tuple funs str",fs )
     `_g` ("prefix_tuple",prefix )
     `_g` ("tuple arr",arr )


    where

    repsToPassToTuple :: RepName -> Maybe RepName
    repsToPassToTuple "SetExplicitVarSizeWithMarker" = Just "unwrapBranch£"
    repsToPassToTuple "SetExplicitVarSize"           = Just "unwrapBranch£"
    repsToPassToTuple _                              = Nothing

    handleTuplesOfMatrixes :: E -> VarData
    handleTuplesOfMatrixes f |  Just num <- prefix `M.lookup` set =
       vdata num $ handleTuplesOfMatrixes' num f

    handleTuplesOfMatrixes f = vdata (0 :: Int) f

    handleTuplesOfMatrixes' :: Int  -> E -> E
    handleTuplesOfMatrixes' 0 f = f
    handleTuplesOfMatrixes' 1 f = reverseTuplesOfMatrixes f
    {-handleTuplesOfMatrixes' 2 f =
        wrapInMatrix . map reverseTuplesOfMatrixes . unwrapMatrix . reverseTuplesOfMatrixes $ f-}
    handleTuplesOfMatrixes' n f =
        wrapInMatrix . map (handleTuplesOfMatrixes' (n - 1)) . unwrapMatrix $ reverseTuplesOfMatrixes f



    -- very hackish but seems to work apart from Matrix1D
    vdata _ e =
        let real = (getVarData mapping prefix (head arr)){vEssence=e}
        in real
        `_p` ("Real VarData",[real]  )

    getVarData :: VarMap -> [String] -> Tree String  -> VarData
    getVarData m2 pre2 (Leaf p2) = vdata2

       where
       name    = intercalate "_" (pre2 ++ [p2 ])
       lookUpE = fromMaybe (_bugg "fromMaybe: lookUpE getVarData")  . flip M.lookup m2
       vdata2   = lookUpE  name

    getVarData m2 pre2 (Branch p2 a2) =
        getVarData m2 (pre2 ++ [p2])  (repSelector a2)

    getVarData m2 pre2 (Tuple a2) =
        getVarData m2 pre2 (head a2)

evalTree' mapping set fs prefix (Branch name arr) =
    evalTree' mapping set (addRep name) (prefix ++ [name])  (repSelector arr)

    `_p` ("branch ",[name] )

    where
    addRep :: RepName -> [RepName]
    addRep name2 | isBranchRep  name2 = name2 : fs
    addRep _ = fs


repSelector :: [Tree String] -> Tree String
repSelector arr = arr !! (length arr -1)


reverseTuplesOfMatrixes ::  E -> E

reverseTuplesOfMatrixes [xMatch| vs := value.tuple.values |] =
    tracer "\nreverseTuplesOfMatrixes result" $
    wrapInMatrix . map matrixToTuple $ transposeE (tracer "\nreverseTuplesOfMatrixes vs:" vs)

reverseTuplesOfMatrixes e = bug $ "reverseTuplesOfMatrixes called on " <+> pretty e

_bug :: String -> [E] -> t
_bug  s = upBug  ("EvaluateTree: " ++ s)
_bugi :: (Show a) => String -> (a, [E]) -> t
_bugi s = upBugi ("EvaluateTree: " ++ s )
_bugg :: String -> t
_bugg s = _bug s []

