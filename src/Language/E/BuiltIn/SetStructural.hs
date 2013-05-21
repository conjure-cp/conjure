{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.BuiltIn.SetStructural
    ( setStructural
    ) where

import Language.E

-- We want the following rule, but we want it ti work independent of how
-- many things are in the set structural variable.
--
-- conjure> cat files/rules/refns/horizontal/set/quantified-subsetEq-2.rule
--
-- [1500]
--
-- &quan {&i,&j} subsetEq &s , &g . &k
--
--     ~~>
--
--     &quan &i,&j in &s , &i .< &j /\ &g . &k


setStructural :: MonadConjure m => RefnFunc m
setStructural
    [xMatch| quantifier    := quantified.quantifier
           | qs            := quantified.quanVar.structural.set
           | []            := quantified.quanOverDom
           | []            := quantified.quanOverOp.binOp.subsetEq
           | quanOverExpr  := quantified.quanOverExpr
           | [guard]       := quantified.guard
           | body          := quantified.body
           |] =
    let
        newGuard = conjunct
            [ [eMake| &i .< &j |]
            | (i,j) <- zip qs (tail qs)
            ]
        guard' = [eMake| &guard /\ &newGuard |]

        unroll []  = bug "unrollQuantifiers.structural.set"
        unroll [i] =
            [xMake| quantified.quantifier := quantifier
                  | quantified.quanVar.structural.single := [i]
                  | quantified.quanOverDom := []
                  | quantified.quanOverOp.binOp.in := []
                  | quantified.quanOverExpr := quanOverExpr
                  | quantified.guard := [guard']
                  | quantified.body := body
                  |]
        unroll (i:is) =
            [xMake| quantified.quantifier := quantifier
                  | quantified.quanVar.structural.single := [i]
                  | quantified.quanOverDom := []
                  | quantified.quanOverOp.binOp.in := []
                  | quantified.quanOverExpr := quanOverExpr
                  | quantified.guard.emptyGuard := []
                  | quantified.body := [unroll is]
                  |]
    in return $ Just [("builtIn.setStructural", unroll qs)]
setStructural _ = return Nothing


