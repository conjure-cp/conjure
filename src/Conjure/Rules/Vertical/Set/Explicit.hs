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
    theRule (Comprehension body gensOrFilters) = do
        (gofBefore, (pat, s), gofAfter) <- matchFirst gensOrFilters $ \ gof -> case gof of
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
