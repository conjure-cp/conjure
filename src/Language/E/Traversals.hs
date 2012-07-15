{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.E.Traversals where

import Stuff.CompT
import Stuff.Generic
import Language.E.Definition

bottomUp :: Monad m => (E -> CompE m E) -> E -> CompE m E
bottomUp post [xMatch| [Prim (S x)] := quantified.quantifier.reference
                     | [dom] := quantified.quanOverDom
                     |] = do
    
bottomUp post p = post p
