{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
module Language.E.NormaliseSolution (normaliseSolution) where

import Language.E
import Language.E.Up.IO

{-
  Normalise a spec e.g
  { {6,5,4}, {1,2,3}, {3,5,8} }
  to
  {{1, 2, 3}, {3, 5, 8}, {4, 5, 6}}
-}

normaliseSolution :: Spec -> Spec
normaliseSolution (Spec v e) =
    let es  = statementAsList e
        res = map normaliseSolution' es
    in  Spec v (listAsStatement res)


normaliseSolution' :: E -> E
normaliseSolution' [xMatch|  [val] := topLevel.letting.expr
                  |  name  := topLevel.letting.name |] =
    let res = normaliseSolution' val
    in  [xMake| topLevel.letting.expr := [res]
              | topLevel.letting.name := name |]

normaliseSolution' [xMatch| vs := value.set.values |] =
    let res = map normaliseSolution' vs
    in  [xMake| value.set.values := sort res |]

normaliseSolution' [xMatch| vs := value.mset.values |] =
    let res = map normaliseSolution' vs
    in  [xMake| value.mset.values := sort res |]


normaliseSolution' [xMatch| vs := value.function.values |] =
    let res = map normaliseSolution' vs
    in  [xMake| value.function.values := sort res |]

normaliseSolution' [xMatch| vs := mapping |] = 
    let res = map normaliseSolution' vs
    in  [xMake| mapping := res |]


normaliseSolution' [xMatch| vs := value.partition |] =
    let res = map normaliseSolution' vs
    in  [xMake| value.partition:= sort res |]

normaliseSolution' [xMatch| vs := part |] = 
    let res = map normaliseSolution' vs
    in  [xMake| part := sort res |]


normaliseSolution' [xMatch| vs := value.matrix.values |] =
    let res = map normaliseSolution' vs
    in  [xMake| value.matrix.values := res |]


normaliseSolution' e = e


_t :: FilePath -> IO()
_t file = do
   ss <- getSpec file
   (print . pretty) $ normaliseSolution ss
