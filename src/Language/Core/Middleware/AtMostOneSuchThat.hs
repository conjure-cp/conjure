{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Middleware.AtMostOneSuchThat ( worker ) where

-- transform a spec so that it contains only one "such that" statement.

import Language.Core.Imports
import Language.Core.Definition
import Language.Core.Middleware

worker :: Monad m => Middleware m Spec Spec
worker (Spec lang statements) = do
    (others,suchthats) <- execWriterT $
            forM statements $ \ st ->
                case isSuchThat st of
                    Nothing -> tell ( [st] , [] )
                    Just xs -> tell ( []   , xs )
    return $ Spec lang $ others ++ toSuchThat suchthats

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
