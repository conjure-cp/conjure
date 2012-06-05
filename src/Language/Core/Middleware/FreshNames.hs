{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Middleware.FreshNames where

-- if a rewrite rule introduces new names, those names need to be unique.

import Language.Core.Imports
import Language.Core.Definition
import Language.Core.Middleware

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Generics.Uniplate.Data ( transform, universe )

worker :: Monad m => Middleware (CompT m) Core Core
worker param = do
    let
        allvars  = S.fromList [ r | R r <- universe param
                                  , r `notElem` ["forAll","exists","sum"]
                                  ]
        metavars = S.fromList [ r | Expr ":metavar" [R r] <- universe param ]
        newvars  = S.difference allvars metavars
    uniqueNames <- forM (S.toList newvars) $ \ a -> do b <- nextUniqueName; return (a,Reference b)
    let uniqueNamesMap = M.fromList uniqueNames
    let
        f p@(R r) = case M.lookup r uniqueNamesMap of
                        Nothing -> p
                        Just t  -> R t
        f p = p
    return $ transform f param
--         
--     
-- renameQuantifiers p@(L {}) = return p
-- renameQuantifiers p@(R {}) = return p
-- renameQuantifiers p@( viewDeep [":expr-quantified"] -> Just xs ) =
--     case lookUpInExpr ":expr-quantified-quanVar" xs of
--         Just [quanVar] -> do
--             newName <- nextUniqueName
--             let
--                 f R 
--             return $ transform f p
--         _ -> err $ "invariant violation in renameQuantifiers" <+> pretty p
-- 
-- worker :: Monad m => Middleware m Spec Spec
-- worker (Spec lang statements) = do
--     (others,suchthats) <- execWriterT $
--             forM statements $ \ st ->
--                 case isSuchThat st of
--                     Nothing -> tell ( [st] , [] )
--                     Just xs -> tell ( []   , xs )
--     return $ Spec lang $ others ++ toSuchThat suchthats
-- 
-- isSuchThat :: Core -> Maybe [Core]
-- isSuchThat (viewDeep [":toplevel",":suchthat"] -> Just xs) = Just xs
-- isSuchThat _ = Nothing
-- 
-- toSuchThat :: [Core] -> [Core]
-- toSuchThat [] = []
-- toSuchThat xs = [ Expr ":toplevel" [Expr ":suchthat" xs] ]
