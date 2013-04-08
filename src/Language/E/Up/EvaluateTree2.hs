{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}

module Language.E.Up.EvaluateTree2 (
    evalTree
) where

import Language.E

import Language.E.Up.Data
import Language.E.Up.Common(transposeE,matrixToTuple,unwrapExpr,wrapInExpr,unwrapMatrix)
import Language.E.Up.Debug

import Data.Map(Map)
import qualified Data.Map as M
import Data.Set(Set)
import qualified Data.Set as S

type VarMap             = Map String VarData
type IsTuplesOfMatrixes = Set [String]

evalTree ::VarMap -> IsTuplesOfMatrixes -> Tree String  -> (String,E)
evalTree mapping set (Branch name arr) =
    (name,evalTree' mapping set [name] (repSelector arr))

evalTree mapping set tree@(Leaf name) =
    (name,  evalTree' mapping set []  tree)

evalTree _ _ _ = _bugg "evalTree no match"

evalTree' :: VarMap ->IsTuplesOfMatrixes -> [String] -> Tree String  -> E
evalTree' mapping set prefix (Leaf part) =
    repConverter part vdata
     `_f` (name, vdata )

    where
    name    = intercalate "_" (prefix ++ [part])
    lookUpE = fromMaybe (_bugg "fromMaybe: lookUpE evalTree'")  . flip M.lookup mapping
    vdata   = lookUpE  name

evalTree' mapping set prefix (Branch part arr) =
    evalTree' mapping set (prefix ++ [part])  (repSelector arr)

     `_g` ("prefix",prefix )


evalTree' mapping set prefix (Tuple arr) =
    let items =  map (unwrapExpr . evalTree' mapping set prefix ) arr
    in  [xMake| expr.value.tuple.values := items |]

     `_g` ("prefix_tuple",prefix )
     `_p` ("items",items )


--evalTree' m prefix tree =  error . show $  (pretty . groom) prefix  <+> (pretty . groom) m  <+>  (pretty . groom) tree


-- Deals with (most) representations
repConverter ::  String -> VarData -> E
repConverter  kind  vdata@VarData{vEssence = es} =
    case kind of
      --"Explicit"   -> explicitRep vdata
      --"Occurrence" -> occurrenceRep vdata
      --"Matrix1D"   -> matrix1DRep (vIndexes vdata) (vEssence vdata)
      --"RelationIntMatrix2" -> relationIntMatrix2Rep vdata
      --"ExplicitVarSizeWithDefault" -> explicitVarSizeWithDefaultRep vdata
      _            -> es



repSelector :: [Tree String] -> Tree String
repSelector arr = arr !! (length arr -1)


reverseTuplesOfMatrixes ::  E -> E
reverseTuplesOfMatrixes [xMatch| vs := value.tuple.values |] =
    wrapInMatrix . map matrixToTuple $ transposeE vs

reverseTuplesOfMatrixes e = bug $ "reverseTuplesOfMatrixes called on " <+> pretty e


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


