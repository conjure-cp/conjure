{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}

module Language.E.Up.Common(
    transposeE,
    unwrapMatrix,
    unwrapMatrix',
    unwrapExpr,
    matrixToTuple,
    wrapInExpr,
    wrapInMatrix,
    toIntLit,
    unwrapValues,
    wrapInFunction
) where

import qualified Data.List as L(transpose)

import Language.E
import Language.E.Up.Debug


transposeE :: [E] -> [E]
{-transposeE e | 1 == 3  `_p` ("tranposeE args", e ) = undefined-}

transposeE arr  |  all (not . isLiteral) arr =
    let arr2D = map tranposeCheck arr
        res = L.transpose arr2D
    in  map wrapInMatrix res

transposeE e = e


tranposeCheck :: E -> [E]
tranposeCheck   [xMatch| vs := value.matrix.values |] = vs
        `_p` ("tranposeCheck mat", vs)

-- This is actually needed for very few cases such as tupley26
tranposeCheck e@[xMatch| _ := value.tuple.values  |] =
    let res = convertTuples e
    in  unwrapMatrix res
        `_p` ("USING tranposeCheck tuples", [res])

tranposeCheck e = _bug "tranposeCheck" [e]


toIntLit :: Integer -> E
toIntLit j = [xMake| value.literal := [Prim (I j)] |]

unwrapValues ::  E -> [E]
unwrapValues  (Tagged "values" vs) =  vs
unwrapValues e = _bug "unwrapValues failed" [e]

wrapInFunction :: [E] -> E
wrapInFunction es = [xMake| value.function.values := es |]


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
unwrapExpr e = _bug "unwrapExpr failed" [e]

unwrapMatrix :: E -> [E]
unwrapMatrix [xMatch| vs := value.matrix.values |] = vs
unwrapMatrix e = _bug "unwrapMatrix failed" [e]

unwrapMatrix' :: String -> E -> [E]
unwrapMatrix' _ [xMatch| vs := value.matrix.values |] = vs
unwrapMatrix' str e = _bug (str ++ ": unwrapMatrix failed") [e]

wrapInMatrix :: [E] -> E
wrapInMatrix arr = [xMake| value.matrix.values := arr |]

matrixToTuple :: E -> E
matrixToTuple [xMatch| vs := value.matrix|] = [xMake| value.tuple := vs |]
matrixToTuple e = _bug "matrixToTuple failed" [e]

wrapInExpr :: E -> E
wrapInExpr e = [xMake| expr := [e] |]


_bug :: String -> [E] -> t
_bug  s = upBug  ("Up.Common: " ++ s)
_bugi :: (Show a) => String -> (a, [E]) -> t
_bugi s = upBugi ("Up.Common: " ++ s )
_bugg :: String -> t
_bugg s = _bug s []

