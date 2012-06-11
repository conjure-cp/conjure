{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Middleware.AtMostOneSuchThat ( worker ) where

-- transform a spec so that it contains only one "such that" statement.

import Language.Core.Imports
import Language.Core.Definition


worker :: Monad m => Spec -> CompT m Spec
worker (Spec lang statements) = return $ Spec lang $ others ++ toSuchThat suchthats
    where
        f [] = ([],[])
        f (x:xs) = case isSuchThat x of
                    Nothing -> ([x],[]) `mappend` f xs
                    Just ys -> ([], ys) `mappend` f xs
        
        (others,suchthats) = f statements

isSuchThat :: Core -> Maybe [Core]
isSuchThat (viewDeep [":toplevel",":suchthat"] -> Just xs) = Just xs
isSuchThat _ = Nothing

toSuchThat :: [Core] -> [Core]
toSuchThat [] = []
toSuchThat xs = [ Expr ":toplevel" [Expr ":suchthat" xs'] ]
    where
        xs' = concatMap conjunctOut xs
        conjunctOut ( viewDeep [":operator-/\\"] -> Just [a,b] ) = conjunctOut a ++ conjunctOut b
        conjunctOut a = [a]
