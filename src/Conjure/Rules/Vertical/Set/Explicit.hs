{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Set.Explicit where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Language.Domain
import Conjure.Language.DomainOf
import Conjure.Language.TypeOf
import Conjure.Language.TH
import Conjure.Language.Lenses

import Conjure.Rules.Definition ( Rule(..), namedRule, representationOf, matchFirst )

import Conjure.Representations ( downX1 )


rule_Comprehension :: Rule
rule_Comprehension = "set-comprehension{Explicit}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, s), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} s) -> return (pat, s)
            _ -> na "rule_Comprehension"
        TypeSet{}            <- typeOf s
        "Explicit"           <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for set-comprehension, Explicit representation"
            , \ fresh ->
                let (jPat, j) = quantifiedVar (fresh `at` 0)
                    val = [essence| &m[&j] |]
                in
                    Comprehension (upd val body)
                        $  gofBefore
                        ++ [ Generator (GenDomainNoRepr jPat index) ]
                        ++ transformBi (upd val) gofAfter
            )
    theRule _ = na "rule_Comprehension"


rule_PowerSet_Comprehension :: Rule
rule_PowerSet_Comprehension = "set-powerSet-comprehension{Explicit}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (setPat, setPatNum, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr setPat@(AbsPatSet pats) expr) -> return (setPat, length pats, expr)
            _ -> na "rule_PowerSet_Comprehension"
        s                    <- match opPowerSet expr
        TypeSet{}            <- typeOf s
        "Explicit"           <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        let upd val old = lambdaToFunction setPat old val
        return
            ( "Vertical rule for set-comprehension, Explicit representation"
            , \ fresh ->
                let outPats =
                        [ quantifiedVar (fresh `at` fromIntegral i) | i <- take setPatNum allNats ]
                    val = AbstractLiteral $ AbsLitSet
                        [ [essence| &m[&j] |] | (_,j) <- outPats ]
                in
                    Comprehension (upd val body) $ concat
                        [ gofBefore
                        , concat
                            [ [ Generator (GenDomainNoRepr pat index) ]
                            | (pat,_) <- take 1 outPats
                            ]
                        , concat
                            [ [ Generator (GenDomainNoRepr pat index)
                              , Condition [essence| &patX > &beforeX |]
                              ]
                            | ((_, beforeX), (pat, patX)) <- zip outPats (tail outPats)
                            ]
                        , transformBi (upd val) gofAfter
                        ]
            )
    theRule _ = na "rule_PowerSet_Comprehension"


rule_Card :: Rule
rule_Card = "set-card{Explicit}" `namedRule` theRule where
    theRule p = do
        s                                         <- match opTwoBars p
        TypeSet{}                                 <- typeOf s
        "Explicit"                                <- representationOf s
        DomainSet _ (SetAttr (SizeAttr_Size n)) _ <- domainOf s
        return ( "Vertical rule for set cardinality, Explicit representation."
               , const n
               )
