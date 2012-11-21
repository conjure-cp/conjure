{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.E.Helpers where

import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import Language.E.TH



conjunct :: [E] -> E
conjunct []     = [eMake| true |]
conjunct [x]    = x
conjunct (x:xs) = let y = conjunct xs in [eMake| &x /\ &y |]


disjunct :: [E] -> E
disjunct []     = [eMake| false |]
disjunct [x]    = x
disjunct (x:xs) = let y = disjunct xs in [eMake| &x \/ &y |]


traceBindings :: MonadConjure m => String -> m ()
traceBindings msg = do
    bs <- gets binders
    trace (msg ++ " " ++ show [ nm | Binder nm _ <- bs ]) (return ())


freshQuanVar :: MonadConjure m => m (Text, E)
freshQuanVar = do
    quanVarStr <- nextUniqueName
    let quanVar = [xMake| structural.single.reference := [Prim $ S quanVarStr] |]
    return (quanVarStr, quanVar)

inForAll :: Text -> E -> E -> E
inForAll quanVar quanOverDom body =
    [xMake| quantified.quantifier.reference                := [Prim $ S "forAll"]
          | quantified.quanVar.structural.single.reference := [Prim $ S quanVar ]
          | quantified.quanOverDom                         := [quanOverDom]
          | quantified.quanOverOp                          := []
          | quantified.quanOverExpr                        := []
          | quantified.guard.emptyGuard                    := []
          | quantified.body                                := [body]
          |]

inForAlls :: [(Text,E)] -> E -> E
inForAlls = go . reverse
    where
        go []         body = body
        go ((i,j):ks) body = go ks $ inForAll i j body

