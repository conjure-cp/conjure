
-- possible bug in the derivation of UniplateDirect.
-- to reproduce:
-- derive DeriveFail.hs --append
-- ghci DeriveFail.hs

-- 3 of the below constructors cause problems: `Tuple`, `LTuple`, `LList`

-- I tried deriving UniplateDirect for (Expr,Expr) which seemed what ghc was
-- looking for for the `Tuple` case, no luck.

import Data.Generics.Uniplate.Direct

data Expr = S String | I Int | B Bool
          | Tuple (Expr,Expr)
          | LTuple [(Expr,Expr)]
          | List [Expr]
          | LList [[Expr]]
    deriving (Eq, Ord, Read, Show)

{-!
deriving instance UniplateDirect Expr
!-}
-- GENERATED START

 
instance Uniplate Expr where
         
        {-# INLINE uniplate #-}
        uniplate (Tuple x1) = plate Tuple |+ x1
        uniplate (LTuple x1) = plate LTuple ||+ x1
        uniplate (List x1) = plate List ||* x1
        uniplate (LList x1) = plate LList ||+ x1
        uniplate x = plate x
-- GENERATED STOP
