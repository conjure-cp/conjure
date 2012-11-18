{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.NoGuards where

import Language.E
import Language.E.Evaluator.Partial ( guardOp )


conjureNoGuards
    :: MonadConjure m
    => Spec
    -> m Spec
conjureNoGuards = noGuardsSpec


noGuardsSpec :: MonadConjure m => Spec -> m Spec
noGuardsSpec = bottomUpSpec' noGuardsE


noGuardsE :: MonadConjure m => E -> m E
noGuardsE
    [xMatch| [Prim (S quantifier)] := quantified.quantifier.reference
           | [quanVar]             := quantified.quanVar
           | quanOverDoms          := quantified.quanOverDom
           | quanOverOps           := quantified.quanOverOp
           | quanOverExprs         := quantified.quanOverExpr
           | guards                := quantified.guard
           | [body]                := quantified.body
           |] = do

    let newGuard = [xMake| emptyGuard := [] |]

    newBody <- case guards of
                    [ [xMatch| [] := emptyGuard |] ] -> return body
                    gs -> guardOp quantifier gs body

    return [xMake| quantified.quantifier.reference  := [Prim (S quantifier)]
                 | quantified.quanVar               := [quanVar]
                 | quantified.quanOverDom           := quanOverDoms
                 | quantified.quanOverOp            := quanOverOps
                 | quantified.quanOverExpr          := quanOverExprs
                 | quantified.guard                 := [newGuard]
                 | quantified.body                  := [newBody]
                 |]
noGuardsE p = return p
