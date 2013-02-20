{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
module Language.E.Normalise (normalise) where

import Language.E
import Language.E.Up.IO

{-
  Normalise a spec e.g
  { {6,5,4}, {1,2,3}, {3,5,8} }
  to
  {{1, 2, 3}, {3, 5, 8}, {4, 5, 6}}
-}

normalise :: Spec -> Spec
normalise (Spec v e) =
    let es  = statementAsList e
        res = map normalise' es
    in  Spec v (listAsStatement res)


normalise' :: E -> E
normalise' [xMatch|  [val] := topLevel.letting.expr
                  |  name  := topLevel.letting.name |] =
    let res = normalise' val
    in  [xMake| topLevel.letting.expr := [res]
              | topLevel.letting.name := name |]

normalise' [xMatch| vs := value.set.values |] =
    let res = map normalise' vs
    in  [xMake| value.set.values := sort res |]

normalise' [xMatch| vs := value.mset.values |] =
    let res = map normalise' vs
    in  [xMake| value.mset.values := sort res |]


normalise' [xMatch| vs := value.function.values |] =
    let res = map normalise' vs
    in  [xMake| value.function.values := sort res |]

normalise' [xMatch| vs := mapping |] = 
    let res = map normalise' vs
    in  [xMake| mapping := res |]


normalise' [xMatch| vs := value.partition |] =
    let res = map normalise' vs
    in  [xMake| value.partition:= sort res |]

normalise' [xMatch| vs := part |] = 
    let res = map normalise' vs
    in  [xMake| part := sort res |]


normalise' [xMatch| vs := value.matrix.values |] =
    let res = map normalise' vs
    in  [xMake| value.set.values := res |]


normalise' e = e

_t :: FilePath -> IO()
_t file = do
   ss <- getSpec file
   (print . pretty) $ normalise ss
