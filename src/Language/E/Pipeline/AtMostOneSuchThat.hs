{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat ) where

-- transform a spec so that it contains only one "such that" statement.

import Language.E


atMostOneSuchThat :: Bool -> Spec -> Spec
atMostOneSuchThat generateEmptySuchThat (Spec lang statements)
    = Spec lang $ listAsStatement $ others ++ toSuchThat generateEmptySuchThat suchthats
    where
        f [] = ([],[])
        f (x:xs) = case isSuchThat x of
                    Nothing -> ([x],[]) `mappend` f xs
                    Just ys -> ([], ys) `mappend` f xs

        (others,suchthats) = f (statementAsList statements)

isSuchThat :: E -> Maybe [E]
isSuchThat [xMatch| xs := topLevel.suchThat |] = Just xs
isSuchThat _ = Nothing

toSuchThat :: Bool -> [E] -> [E]
toSuchThat generateEmptySuchThat xs =
    let
        constraints
            = sort
            $ nubKeepOrder
            $ filter (/= trueCons)
            $ concatMap conjunctOut xs
        trueCons = [eMake| true |]
    in  if null constraints
            then
                if generateEmptySuchThat
                    then [ [xMake| topLevel.suchThat := [trueCons] |] ]
                    else []
            else [ [xMake| topLevel.suchThat := constraints |] ]
    where
        conjunctOut :: E -> [E]
        conjunctOut [eMatch| &a /\ &b |] = conjunctOut a ++ conjunctOut b
        conjunctOut a = [a]

