{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.E.Helpers where

import Language.E.Imports
import Language.E.Definition
import Language.E.Pretty
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


bindersDoc :: MonadConjure m => m Doc
bindersDoc = do
    bs <- gets binders
    return $ prettyList id "," $ nub [ nm | Binder nm _ <- bs ]


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

