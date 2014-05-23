{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.CategoryOf ( Category(..), categoryOf ) where

import Language.E.Imports
import Language.E.Definition
import Language.E.CompE


data Category = CatConstant | CatParameter | CatQuantified | CatDecision
    deriving (Eq, Ord, Show)

categoryOf :: MonadConjure m => E -> m Category

categoryOf [xMatch| [Prim (S i)] := reference |] =
    if i `elem` ["_", "forAll", "exists", "sum"]
        then return CatConstant
        else do
            x <- errMaybeT "categoryOf" lookupReference i
            categoryOf x

categoryOf [xMatch| _ := topLevel.declaration.find  |] = return CatDecision
categoryOf [xMatch| _ := topLevel.declaration.given |] = return CatParameter
categoryOf [xMatch| _ := quanVar.within             |] = return CatQuantified

categoryOf Prim {} = return CatConstant
categoryOf (Tagged _ []) = return CatConstant
categoryOf (Tagged _ xs) = maximum <$> mapM categoryOf xs
categoryOf (D d) = maximum <$> mapM categoryOf (universeBi d)
categoryOf EOF {} = return CatConstant
categoryOf (StatementAndNext a b) = maximum <$> mapM categoryOf [a,b]

universeBi :: Domain -> [E]
universeBi = undefined
