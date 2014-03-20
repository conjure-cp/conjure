{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}

module Language.E.NormaliseSolution ( normaliseSolution, normaliseSolutionEs ) where

import Language.E
import Language.E.Up.IO

{-
  Normalise a spec e.g
  { {6,5,4}, {1,2,3}, {3,5,8} }
  to
  {{1, 2, 3}, {3, 5, 8}, {4, 5, 6}}
-}

normaliseSolution :: Spec -> Spec
normaliseSolution (Spec v e) = Spec v (normaliseSolutionE e)

normaliseSolutionEs :: [E] -> [E]
normaliseSolutionEs = map normaliseSolutionE

normaliseSolutionE :: E -> E
normaliseSolutionE [xMatch| [val] := topLevel.letting.expr
                          | name  := topLevel.letting.name |] =
    let res = normaliseSolutionE val
    in  [xMake| topLevel.letting.expr := [res]
              | topLevel.letting.name := name |]


normaliseSolutionE [xMatch| vs := value.set.values |] =
    let res = map normaliseSolutionE vs
    in  [xMake| value.set.values := sort res |]

normaliseSolutionE [xMatch| vs := value.mset.values |] =
    let res = map normaliseSolutionE vs
    in  [xMake| value.mset.values := sort res |]

normaliseSolutionE [xMatch| vs := value.relation.values |] =
    let res = map normaliseSolutionE vs
    in  [xMake| value.relation.values := sort res |]

normaliseSolutionE [xMatch| vs := value.function.values |] =
    let res = map normaliseSolutionE vs
    in  [xMake| value.function.values := sort res |]

normaliseSolutionE [xMatch| vs := mapping |] =
    let res = map normaliseSolutionE vs
    in  [xMake| mapping := res |]


normaliseSolutionE [xMatch| vs := value.partition.values |] =
    let res = map normaliseSolutionE vs
    in  [xMake| value.partition.values:= sort res |]

normaliseSolutionE [xMatch| vs := part |] =
    let res = map normaliseSolutionE vs
    in  [xMake| part := sort res |]


normaliseSolutionE [xMatch| vs   := value.matrix.values
                          | [ir] := value.matrix.indexrange
                          |] =
    let res = map normaliseSolutionE vs
        ir' = normaliseSolutionE ir
    in  [xMake| value.matrix.values := res
              | value.matrix.indexrange := [ir']
              |]

normaliseSolutionE [xMatch| vs := value.matrix.values |] =
    let res = map normaliseSolutionE vs
    in  [xMake| value.matrix.values := res |]

normaliseSolutionE [xMatch| as := attrCollection |] = [xMake| attrCollection := bs |]
    where bs = sortBy (comparing attributeName) as
          attributeName [xMatch| [n] := attribute.nameValue.name |] = Just n
          attributeName [xMatch| [n] := attribute.name           |] = Just n
          attributeName _ = Nothing

normaliseSolutionE (Tagged t xs) = Tagged t (map normaliseSolutionE xs)
normaliseSolutionE e = e


_t :: FilePath -> IO()
_t file = do
   ss <- getSpec file
   (print . pretty) $ normaliseSolution ss
