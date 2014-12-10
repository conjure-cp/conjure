{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Set.ExplicitVarSizeWithMarker where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Language.Domain
import Conjure.Language.DomainOf
import Conjure.Language.TypeOf
import Conjure.Language.Lenses
import Conjure.Language.TH

import Conjure.Rules.Definition ( Rule(..), namedRule, representationOf, matchFirst )

import Conjure.Representations ( downX1 )


rule_Comprehension :: Rule
rule_Comprehension = "set-comprehension{ExplicitVarSizeWithMarker}" `namedRule` theRule where
    theRule (Comprehension body gensOrFilters) = do
        (gofBefore, (pat, s), gofAfter) <- matchFirst gensOrFilters $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} s) -> return (pat, s)
            _ -> na "rule_Comprehension"
        TypeSet{}                   <- typeOf s
        "ExplicitVarSizeWithMarker" <- representationOf s
        [marker, values]            <- downX1 s
        DomainMatrix index _        <- domainOf values
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for set-comprehension, ExplicitVarSizeWithMarker representation"
            , \ fresh ->
                let (jPat, j) = quantifiedVar (fresh `at` 0)
                    val = [essence| &values[&j] |]
                in
                    Comprehension (upd val body)
                        $  gofBefore
                        ++ [ Generator (GenDomainNoRepr jPat index)
                           , Filter [essence| &j <= &marker |]
                           ]
                        ++ transformBi (upd val) gofAfter
               )
    theRule _ = na "rule_Comprehension"


rule_Card :: Rule
rule_Card = "set-card{ExplicitVarSizeWithMarker}" `namedRule` theRule where
    theRule p = do
        s                           <- match opTwoBars p
        TypeSet{}                   <- typeOf s
        "ExplicitVarSizeWithMarker" <- representationOf s
        [marker, _values]           <- downX1 s
        return ( "Vertical rule for set cardinality, ExplicitVarSizeWithMarker representation."
               , const marker
               )
