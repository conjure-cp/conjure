{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.NoGuards where

import Language.E
import Language.E.Evaluator.Partial ( guardOp )


conjureNoGuards :: (Monad m, Functor m)
    => Spec
    -> CompE m Spec
conjureNoGuards = noGuardsSpec


noGuardsSpec :: (Functor m, Monad m) => Spec -> CompE m Spec
noGuardsSpec = traverseSpec' noGuardsE


noGuardsE :: (Functor m, Monad m) => E -> CompE m E
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
