{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.MSet.ExplicitWithFlags where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "mset-comprehension{ExplicitWithFlags}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, s), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} s) -> return (pat, s)
            _ -> na "rule_Comprehension"
        TypeMSet{}             <- typeOf s
        MSet_ExplicitWithFlags <- representationOf s
        [flags, values]        <- downX1 s
        DomainMatrix index _   <- domainOf values
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for mset-comprehension, ExplicitWithFlags representation"
            , do
                (jPat, j) <- quantifiedVar
                let val = [essence| &values[&j] |]
                return $ Comprehension (upd val body)
                        $  gocBefore
                        ++ [ Generator (GenDomainNoRepr jPat index)
                           , Condition [essence| &flags[&j] > 0 |]
                           ]
                        ++ transformBi (upd val) gocAfter
               )
    theRule _ = na "rule_Comprehension"


rule_Freq :: Rule
rule_Freq = "mset-freq{ExplicitWithFlags}" `namedRule` theRule where
    theRule p = do
        (mset, x)              <- match opFreq p
        TypeMSet{}             <- typeOf mset
        MSet_ExplicitWithFlags <- representationOf mset
        [flags, values]        <- downX1 mset
        DomainMatrix index _   <- domainOf values
        return
            ( "Vertical rule for mset-freq, ExplicitWithFlags representation"
            , do
                (iPat, i) <- quantifiedVar
                return
                    [essence|
                        sum([ &flags[&i]
                            | &iPat : &index
                            , &values[&i] = &x
                            ])
                    |]
            )
