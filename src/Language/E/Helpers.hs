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


domainNeedsRepresentation :: E -> Bool
domainNeedsRepresentation [xMatch|  _  := domain.set          |] = True
domainNeedsRepresentation [xMatch|  _  := domain.mset         |] = True
domainNeedsRepresentation [xMatch|  _  := domain.function     |] = True
domainNeedsRepresentation [xMatch|  _  := domain.relation     |] = True
domainNeedsRepresentation [xMatch|  _  := domain.partition    |] = True
domainNeedsRepresentation [xMatch| [i] := domain.matrix.inner |] = domainNeedsRepresentation i
domainNeedsRepresentation _ = False


freshQuanVar :: MonadConjure m => m (Text, E)
freshQuanVar = do
    quanVarStr <- nextUniqueName
    let quanVar = [xMake| structural.single.reference := [Prim $ S quanVarStr] |]
    return (quanVarStr, quanVar)

inForAll :: Text -> E -> E -> E
inForAll quanVar quanOverDom body =
    let
        out = 
            [xMake| quantified.quantifier.reference                := [Prim $ S "forAll"]
                  | quantified.quanVar.structural.single.reference := [Prim $ S quanVar ]
                  | quantified.quanOverDom                         := [quanOverDom]
                  | quantified.quanOverOp                          := []
                  | quantified.quanOverExpr                        := []
                  | quantified.guard.emptyGuard                    := []
                  | quantified.body                                := [body]
                  |]
    in  fromMaybe out (tryUnrollForAll out)

inForAlls :: [(Text,E)] -> E -> E
inForAlls = go . reverse
    where
        go []         body = body
        go ((i,j):ks) body = go ks $ inForAll i j body

tryUnrollForAll :: E -> Maybe E
tryUnrollForAll
    [xMatch| [Prim (S "forAll")] := quantified.quantifier.reference
           | [Prim (S quanVar)]  := quantified.quanVar.structural.single.reference
           | ranges              := quantified.quanOverDom.domain.int.ranges
           | []                  := quantified.quanOverOp
           | []                  := quantified.quanOverExpr
           | []                  := quantified.guard.emptyGuard
           | [body]              := quantified.body
           |] = do
    let
        valueIntFrom [xMatch| [Prim (I i)] := value.literal |] = Just i
        valueIntFrom _ = Nothing

        collectInts [xMatch| [ i ] := range.single |] = Just [i]
        collectInts [xMatch| [i,j] := range.fromTo |] = do
            iInt <- valueIntFrom i
            jInt <- valueIntFrom j
            return $ map (\ x -> [xMake| value.literal := [Prim (I x)] |] )
                     [iInt .. jInt]
        collectInts _ = Nothing

    ints <- concatMapM collectInts ranges
    return $ conjunct
        [ replace
            [xMake| structural.single.reference := [Prim (S quanVar)] |]
            x
            body
        | x <- ints
        ]
tryUnrollForAll _ = Nothing

