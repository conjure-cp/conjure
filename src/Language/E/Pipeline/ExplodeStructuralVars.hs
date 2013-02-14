{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.ExplodeStructuralVars
    ( explodeStructuralVars
    ) where

import Language.E


explodeStructuralVars :: MonadConjure m => Spec -> m Spec
explodeStructuralVars = bottomUpSpec' helper
    where

        helper x@[xMatch| _ := quantified.quanVar.structural.single |] = return x

        -- inlining a structural variable
        helper
            [xMatch| quantifiers   := quantified.quantifier
                   | [quanVar]     := quantified.quanVar
                   | quanOverDoms  := quantified.quanOverDom
                   | quanOverOps   := quantified.quanOverOp
                   | quanOverExprs := quantified.quanOverExpr
                   | guards        := quantified.guard
                   | bodys         := quantified.body
                   |] = do
            uniq' <- nextUniqueName
            -- the new quanVar
            let uniq = [xMake| structural.single.reference := [Prim (S uniq')] |]
            let mappings = genMappings quanVar
            let replacerFunc = replaceAll [ (old, new)
                                          | (old, is) <- mappings
                                          , let new = mkIndexedExpr (map intToE is) uniq
                                          , old /= [eMake| _ |]
                                          ]
            let result = [xMake| quantified.quantifier   := quantifiers
                               | quantified.quanVar      := [uniq]
                               | quantified.quanOverDom  := quanOverDoms
                               | quantified.quanOverOp   := quanOverOps
                               | quantified.quanOverExpr := quanOverExprs
                               | quantified.guard        := map replacerFunc guards
                               | quantified.body         := map replacerFunc bodys
                               |]
            return result

        helper x = return x


-- i         --> i -> []
-- (i,j)     --> i -> [1]
--               j -> [2]
-- (i,(j,k)) --> i -> [1]
--               j -> [2,1]
--               k -> [2,2]
genMappings :: E -> [(E,[Integer])]
genMappings [xMatch| xs  := structural.tuple  |]
    = concat
        [ [ (y,i:is) | (y,is) <- genMappings x ]
        | (i,x) <- zip [1..] xs
        ]
genMappings [xMatch| xs  := structural.matrix |]
    = concat
        [ [ (y,i:is) | (y,is) <- genMappings x ]
        | (i,x) <- zip [1..] xs
        ]
genMappings x = [(x,[])]


intToE :: Integer -> E
intToE i = [xMake| value.literal := [Prim (I i)] |]


