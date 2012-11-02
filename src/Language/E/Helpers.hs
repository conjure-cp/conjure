{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Helpers where

import Language.E.Definition
import Language.E.TH


conjunct :: [E] -> E
conjunct []     = [eMake| true |]
conjunct [x]    = x
conjunct (x:xs) = let y = conjunct xs in [eMake| &x /\ &y |]


disjunct :: [E] -> E
disjunct []     = [eMake| false |]
disjunct [x]    = x
disjunct (x:xs) = let y = disjunct xs in [eMake| &x \/ &y |]


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

