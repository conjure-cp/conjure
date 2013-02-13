{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.ExplodeStructuralVars
    ( explodeStructuralVars
    ) where

import Language.E


explodeStructuralVars :: MonadConjure m => Spec -> m Spec
explodeStructuralVars = bottomUpSpec' helper
    where

        -- inlining a structural variable, tuple here.

        helper
            [xMatch| quantifiers   := quantified.quantifier
                   | ts            := quantified.quanVar.structural.tuple
                   | quanOverDoms  := quantified.quanOverDom
                   | quanOverOps   := quantified.quanOverOp
                   | quanOverExprs := quantified.quanOverExpr
                   | guards        := quantified.guard
                   | bodys         := quantified.body
                   |] = do
            i' <- nextUniqueName
            let
                -- the new quanVar
                i = [xMake| structural.single.reference := [Prim (S i')] |]

                -- index at position j, an integer.
                indexIt j' = let j = [xMake| value.literal := [Prim (I j')] |]
                             in  [eMake| &i[&j] |]

                -- list of indexed quanVars, instead of the input ts
                indexeds = map indexIt [1 .. genericLength ts]

                replacerFunc = replaceAll [ (old, new)
                                          | (old, new) <- zip ts indexeds
                                          , old /= [eMake| _ |]
                                          ]

            let
                result = [xMake| quantified.quantifier   := quantifiers
                               | quantified.quanVar      := [i]
                               | quantified.quanOverDom  := quanOverDoms
                               | quantified.quanOverOp   := quanOverOps
                               | quantified.quanOverExpr := quanOverExprs
                               | quantified.guard        := map replacerFunc guards
                               | quantified.body         := map replacerFunc bodys
                               |]
            return result

        helper x = return x

