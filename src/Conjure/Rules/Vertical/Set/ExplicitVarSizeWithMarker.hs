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
        (gofBefore, (pat, iPat, s), gofAfter) <- matchFirst gensOrFilters $ \ gof -> case gof of
            Generator (GenInExpr pat@(Single iPat) s) -> return (pat, iPat, s)
            _ -> fail "No match."        
        TypeSet{}                   <- typeOf s
        "ExplicitVarSizeWithMarker" <- representationOf s
        [marker, values]            <- downX1 s
        DomainMatrix index _        <- domainOf values
        let i = Reference iPat Nothing
        let upd val old = lambdaToFunction pat old val
        return ( "Vertical rule for set-comprehension, ExplicitVarSizeWithMarker representation"
               , const $
                    Comprehension (upd [essence| &values[&i] |] body)
                        $  gofBefore
                        ++ [ Generator (GenDomain pat index)
                           , Filter [essence| &i <= &marker |]
                           ]
                        ++ transformBi (upd [essence| &values[&i] |]) gofAfter
               )
    theRule _ = fail "No match."


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
