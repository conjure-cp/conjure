{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Phases.StructuredVars where

import Control.Monad.State ( MonadState, evalState )
import Data.List ( nub )
import qualified Data.Map as M

import Constants
import GenericOps.Core ( bottomUp, bottomUpM, universe )
import Has

import Language.Essence


structuredVars :: Spec -> Spec
structuredVars spec = do
    let qNames = mkFreshNames $ nub [ nm | Identifier nm <- universe spec ]
    evalState (bottomUpM f spec) qNames
    where
        f :: (MonadState st m, Has st [FreshName]) => QuantifiedExpr -> m QuantifiedExpr
        f q@(QuantifiedExpr {quanVar = Right sv}) = do
            newName <- getFreshName
            let mapping = getMapping newName sv
            return $ flip bottomUp q $ \ t -> case t of
                                                EHole (Identifier nm) -> case M.lookup nm mapping of
                                                                            Nothing -> t
                                                                            Just t' -> t'
                                                _ -> t
        f q = return q


-- to test:
-- mapM_ (\ (x,y) -> putStrLn $ x ++ "  " ++ show (pretty y) ) $ M.toList $ getMapping "q" $ pS "(x,[y],[a,b,c])"

-- pS :: String -> StructuredVar
-- pS = unsafeParse parse

getMapping :: String -> StructuredVar -> M.Map String Expr
getMapping new s = M.map ( foldr (\ i n -> EOp Index [n, V $ VInt i ] ) (EHole $ Identifier new) . reverse
                         ) (worker s)
    where
        worker :: StructuredVar -> M.Map String [Integer]
        worker (I (Identifier nm)) = M.singleton nm []
        worker (STuple  vs) = M.unions [ M.map (n:) (worker v)
                                       | (v,n) <- zip vs [1..]
                                       ]
        worker (SMatrix vs) = M.unions [ M.map (n:) (worker v)
                                       | (v,n) <- zip vs [1..]
                                       ]
