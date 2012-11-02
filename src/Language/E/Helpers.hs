{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Helpers where

import Stuff.FunkyT
import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import Language.E.TH


withBindingScope'
    :: Monad m
    => CompE m a
    -> CompE m a
withBindingScope' comp = do
    bindersBefore <- getsLocal binders
    result <- comp
    modifyLocal $ \ st -> st { binders = bindersBefore }
    return result


withBindingScope
    :: ( Monad (t (CompEMOnly m))
       , Monad m
       , MonadTrans t
       )
    => t (CompEMOnly m) a
    -> t (CompEMOnly m) a
withBindingScope comp = do
    bindersBefore <- lift $ getsLocal binders
    result <- comp
    lift $ modifyLocal $ \ st -> st { binders = bindersBefore }
    return result


conjunct :: [E] -> E
conjunct []     = [eMake| true |]
conjunct [x]    = x
conjunct (x:xs) = let y = conjunct xs in [eMake| &x /\ &y |]


disjunct :: [E] -> E
disjunct []     = [eMake| false |]
disjunct [x]    = x
disjunct (x:xs) = let y = disjunct xs in [eMake| &x \/ &y |]


freshQuanVar :: Monad m => CompE m (String, E)
freshQuanVar = do
    quanVarStr <- nextUniqueName
    let quanVar = [xMake| structural.single.reference := [Prim $ S quanVarStr] |]
    return (quanVarStr, quanVar)


inForAll :: String -> E -> E -> E
inForAll quanVar quanOverDom body =
    [xMake| quantified.quantifier.reference                := [Prim $ S "forAll"]
          | quantified.quanVar.structural.single.reference := [Prim $ S quanVar ]
          | quantified.quanOverDom                         := [quanOverDom]
          | quantified.quanOverOp                          := []
          | quantified.quanOverExpr                        := []
          | quantified.guard.emptyGuard                    := []
          | quantified.body                                := [body]
          |]

inForAlls :: [(String,E)] -> E -> E
inForAlls = go . reverse
    where
        go []         body = body
        go ((i,j):ks) body = go ks $ inForAll i j body

