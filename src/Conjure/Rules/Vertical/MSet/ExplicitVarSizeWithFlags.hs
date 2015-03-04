{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.MSet.ExplicitVarSizeWithFlags where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "mset-comprehension{ExplicitVarSizeWithFlags}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, s), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} s) -> return (pat, s)
            _ -> na "rule_Comprehension"
        TypeMSet{}                  <- typeOf s
        "ExplicitVarSizeWithFlags"  <- representationOf s
        [flags, values]             <- downX1 s
        DomainMatrix index _        <- domainOf values
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for mset-comprehension, ExplicitVarSizeWithFlags representation"
            , \ fresh ->
                let (jPat, j) = quantifiedVar (fresh `at` 0)
                    val = [essence| &values[&j] |]
                in
                    Comprehension (upd val body)
                        $  gocBefore
                        ++ [ Generator (GenDomainNoRepr jPat index)
                           , Condition [essence| &flags[&j] > 0 |]
                           ]
                        ++ transformBi (upd val) gocAfter
               )
    theRule _ = na "rule_Comprehension"


rule_Freq :: Rule
rule_Freq = "mset-freq{ExplicitVarSizeWithFlags}" `namedRule` theRule where
    theRule p = do
        (mset, x)                  <- match opFreq p
        TypeMSet{}                 <- typeOf mset
        "ExplicitVarSizeWithFlags" <- representationOf mset
        [flags, values]            <- downX1 mset
        DomainMatrix index _       <- domainOf values
        return ( "Vertical rule for mset-freq, ExplicitVarSizeWithFlags representation"
               , \ fresh ->
                let
                    (iPat, i) = quantifiedVar (fresh `at` 0)
                in
                    [essence|
                        sum([ &flags[&i]
                            | &iPat : &index
                            , &values[&i] = &x
                            ])
                    |]
               )
