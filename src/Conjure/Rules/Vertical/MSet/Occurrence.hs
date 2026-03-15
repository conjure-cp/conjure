{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.MSet.Occurrence where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "mset-comprehension{Occurrence}" `namedRuleZ` theRule where
    theRule z (Comprehension body gensOrConds) = do
        (gocBefore, (pat, s), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} s) -> return (pat, s)
            _ -> na "rule_Comprehension"
        TypeMSet{}             <- typeOf s
        MSet_Occurrence        <- representationOf s
        [m]                    <- downX1 s
        DomainMatrix index _   <- domainOf m
        let upd val old = lambdaToFunction pat old val
        theyDo <- doDuplicatesMatter z
        return
            ( "Vertical rule for mset-comprehension, Occurrence representation"
            , do
                (jPat, j) <- quantifiedVar
                let val = j
                let outBody = upd val body
                return $ Comprehension (if theyDo then [essence| &outBody * &m[&j] |]
                                                    else outBody)
                        $  gocBefore
                        ++ [ Generator (GenDomainNoRepr jPat index)
                            , Condition [essence| &m[&j] > 0 |]
                            ]
                        ++ transformBi (upd val) gocAfter
            )
    theRule _ _ = na "rule_Comprehension"


rule_Freq :: Rule
rule_Freq = "mset-freq{Occurrence}" `namedRule` theRule where
    theRule p = do
        (mset, x)              <- match opFreq p
        TypeMSet{}             <- typeOf mset
        MSet_Occurrence        <- representationOf mset
        [m]                    <- downX1 mset
        return
            ( "Vertical rule for mset-freq, Occurrence representation"
            , return [essence| &m[&x] |]
            )
