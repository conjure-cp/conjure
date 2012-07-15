{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat ) where

-- transform a spec so that it contains only one "such that" statement.

import Language.E


atMostOneSuchThat :: Monad m => Spec -> CompE m Spec
atMostOneSuchThat (Spec lang statements) = return $ Spec lang $ others ++ toSuchThat suchthats
    where
        f [] = ([],[])
        f (x:xs) = case isSuchThat x of
                    Nothing -> ([x],[]) `mappend` f xs
                    Just ys -> ([], ys) `mappend` f xs

        (others,suchthats) = f statements

isSuchThat :: E -> Maybe [E]
isSuchThat [xMatch| xs := topLevel.suchThat |] = Just xs
isSuchThat _ = Nothing

toSuchThat :: [E] -> [E]
toSuchThat [] = []
toSuchThat xs = [ [xMake| topLevel.suchThat := concatMap conjunctOut xs |] ]
    where
        conjunctOut :: E -> [E]
        conjunctOut [xMatch| [Prim (S "/\\")] := binOp.operator
                           | [a]              := binOp.left
                           | [b]              := binOp.right
                           |] = conjunctOut a ++ conjunctOut b
        conjunctOut a = [a]
