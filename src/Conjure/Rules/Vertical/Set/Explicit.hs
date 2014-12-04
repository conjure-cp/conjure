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
        (gofBefore, (pat, iPat, s), gofAfter) <- matchFirst gensOrFilters $ \ gof -> case gof of
            Generator (GenInExpr pat@(Single iPat) s) -> return (pat, iPat, s)
            _ -> na "rule_Comprehension"
        TypeSet{}            <- typeOf s
        "Explicit"           <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        let i = Reference iPat Nothing
        let upd val old = lambdaToFunction pat old val
        return ( "Vertical rule for set-comprehension, Explicit representation"
               , const $ let val = [essence| &m[&i] |] in
                   Comprehension (upd val body)
                       $  gofBefore
                       ++ [ Generator (GenDomain pat index) ]
                       ++ transformBi (upd val) gofAfter
               )
    theRule _ = na "rule_Comprehension"


rule_Card :: Rule
rule_Card = "set-card{Explicit}" `namedRule` theRule where
    theRule p = do
        s                                        <- match opTwoBars p
        TypeSet{}                                <- typeOf s
        "Explicit"                               <- representationOf s
        DomainSet _ (SetAttr (SizeAttrSize n)) _ <- domainOf s
        return ( "Vertical rule for set cardinality, Explicit representation."
               , const n
               )
