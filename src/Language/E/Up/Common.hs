{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}

module Language.E.Up.Common(
    transposeE,
    unwrapMatrix,
    unwrapExpr,
    matrixToTuple
) where

import qualified Data.List as L(transpose)

import Language.E
import Language.E.Up.Debug


transposeE :: [E] -> [E]
transposeE e | 1 == 3  `_p` ("tranposeE args", e ) = undefined

transposeE arr  |  all (not . isLiteral) arr =
    let arr2D = map tranposeCheck arr
        res = L.transpose arr2D
    in  map (\ele -> [xMake| value.matrix.values := ele |] ) res

transposeE e = e


tranposeCheck :: E -> [E]
tranposeCheck   [xMatch| vs := value.matrix.values |] = vs
        `_p` ("tranposeCheck mat", vs)

-- This is actually needed for very few cases such as tupley26
tranposeCheck e@[xMatch| _ := value.tuple.values  |] =
    let res = convertTuples e
    in  unwrapMatrix res
        `_p` ("USING tranposeCheck tuples", [res])

tranposeCheck e = errbM "tranposeCheck" [e]


convertTuples :: E -> E
convertTuples [xMatch| vs := value.tuple.values |] =
    let res  = map matrixToTuple (transposeE vs)
        res' = [xMake| value.matrix.values := res |]
    in res'

convertTuples (e) = e


isLiteral ::  E -> Bool
isLiteral [xMatch| _ := value.literal |] = True
isLiteral  _ = False


unwrapExpr ::  E -> E
unwrapExpr  (Tagged Texpr [val]) =  val
unwrapExpr e = errpM "EvaluateTree: unwrapExpr failed" [e]

unwrapMatrix :: E -> [E]
unwrapMatrix [xMatch| vs := value.matrix.values |] = vs
unwrapMatrix e = errpM "Common: unwrapMatrix failed" [e]


matrixToTuple :: E -> E
matrixToTuple [xMatch| vs := value.matrix|] = [xMake| value.tuple := vs |]
matrixToTuple e = errpM "Common: matrixToTuple failed" [e]


wrapInMatrix :: [E] -> E
wrapInMatrix arr = [xMake| value.matrix.values := arr |]

