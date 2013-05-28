{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.ExplodeStructuralVars
    ( explodeStructuralVars
    ) where

import Language.E


explodeStructuralVars :: MonadConjure m => Spec -> m Spec
explodeStructuralVars = quantificationOverTupleDomains >=> bottomUpSpec' helper
    where

        helper x@[xMatch| _ := quantified.quanVar.structural.single |] = return x

        -- inlining a structural variable
        helper
          x@[xMatch| quantifiers   := quantified.quantifier
                   | [quanVar]     := quantified.quanVar
                   | quanOverDoms  := quantified.quanOverDom
                   | quanOverOps   := quantified.quanOverOp
                   | quanOverExprs := quantified.quanOverExpr
                   | guards        := quantified.guard
                   | bodys         := quantified.body
                   |] =
            case (quanVar, genMappings quanVar) of
                ([xMatch| _ := structural.set |], _) -> return x
                (_, mappings) -> do
                    uniq' <- nextUniqueName
                    -- the new quanVar
                    let uniq = [xMake| structural.single.reference := [Prim (S uniq')] |]
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


quantificationOverTupleDomains :: MonadConjure m => Spec -> m Spec
quantificationOverTupleDomains = bottomUpSpec' helper
    where
        helper
          x@[xMatch| [Prim (S quan)] := quantified.quantifier.reference
                   | [Prim (S nm  )] := quantified.quanVar.structural.single.reference
                   | doms            := quantified.quanOverDom.domain.tuple.inners
                   | []              := quantified.quanOverOp
                   | []              := quantified.quanOverExpr
                   | [guard]         := quantified.guard
                   | [body]          := quantified.body
                   |] = do
            let nmDoms = zip [ mconcat [nm, "_tuple", stringToText (show (i::Int))]
                             | i <- [1..] ]
                             doms
            let replacerFunc = replace [xMake| reference := [Prim (S nm)] |] new
                    where
                        newInner = map (\ (i,_) -> [xMake| reference := [Prim (S i)] |] ) nmDoms
                        new = [xMake| value.tuple.values := newInner |]
            let guard' = replacerFunc guard
            let body'  = replacerFunc body
            let
                go [] = bug "quantificationOverTupleDomains"
                go [(quanVar, quanOverDom)] =
                    inQuan quan quanVar quanOverDom
                        ( guard'
                        , body'
                        )
                go ((quanVar, quanOverDom):rest) =
                    inQuan quan quanVar quanOverDom
                        ( [xMake| emptyGuard := [] |]
                        , (go rest)
                        )
            let out = go nmDoms
            mkLog "builtIn.quantificationOverTupleDomains" $ sep [pretty x, "~~>", pretty out]
            return out
        helper x = return x


