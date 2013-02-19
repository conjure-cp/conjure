{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
module Language.E.Normalise (normalise) where

import Language.E

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

normalise' e = e


