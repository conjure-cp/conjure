{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat ) where

-- transform a spec so that it contains only one "such that" statement.

import Language.E


atMostOneSuchThat :: Spec -> Spec
atMostOneSuchThat (Spec lang statements)
    = Spec lang $ listAsStatement $ others ++ [toSuchThat suchthats]
    where
        f [] = ([],[])
        f (x:xs) = case isSuchThat x of
                    Nothing -> ([x],[]) `mappend` f xs
                    Just ys -> ([], ys) `mappend` f xs

        (others,suchthats) = f (statementAsList statements)

isSuchThat :: E -> Maybe [E]
isSuchThat [xMatch| xs := topLevel.suchThat |] = Just xs
isSuchThat _ = Nothing

toSuchThat :: [E] -> E
toSuchThat xs =
    let
        constraints' = nub $ concatMap conjunctOut xs
        trueCons = [eMake| true |]
        constraints = if null constraints' then [trueCons] else constraints'
    in  [xMake| topLevel.suchThat := constraints |]
    where
        conjunctOut :: E -> [E]
        conjunctOut [eMatch| &a /\ &b |] = conjunctOut a ++ conjunctOut b
        conjunctOut a = [a]

