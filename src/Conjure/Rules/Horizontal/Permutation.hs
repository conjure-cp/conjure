{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.Permutation where

import Conjure.Rules.Import
import Conjure.Rules.Definition

-- uniplate
import Data.Generics.Uniplate.Zipper as Zipper ( up, hole )


-- | want this to be 
-- [_ | (i, j) <- compose(f, g)
--     , ...]
-- becomes
-- [_ | i : int(1..10)
--    , g(i) in defined(f)
--    , letting j be f(g(i))
--    ]
rule_Composition_Comprehension :: Rule
rule_Composition_Comprehension =
  error "permutation: rule_Composition_Comprehension not defined yet" 





