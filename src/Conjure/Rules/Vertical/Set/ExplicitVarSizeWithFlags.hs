{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Set.ExplicitVarSizeWithFlags where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "set-comprehension{ExplicitVarSizeWithFlags}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, s), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} s) -> return (pat, s)
            _ -> na "rule_Comprehension"
        TypeSet{}                   <- typeOf s
        "ExplicitVarSizeWithFlags"  <- representationOf s
        [flags, values]             <- downX1 s
        DomainMatrix index _        <- domainOf values
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for set-comprehension, ExplicitVarSizeWithFlags representation"
            , \ fresh ->
                let (jPat, j) = quantifiedVar (fresh `at` 0)
                    val = [essence| &values[&j] |]
                in
                    Comprehension (upd val body)
                        $  gocBefore
                        ++ [ Generator (GenDomainNoRepr jPat index)
                           , Condition [essence| &flags[&j] |]
                           ]
                        ++ transformBi (upd val) gocAfter
               )
    theRule _ = na "rule_Comprehension"


rule_PowerSet_Comprehension :: Rule
rule_PowerSet_Comprehension = "set-powerSet-comprehension{ExplicitVarSizeWithFlags}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (setPat, setPatNum, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr setPat@(AbsPatSet pats) expr) -> return (setPat, length pats, expr)
            _ -> na "rule_PowerSet_Comprehension"
        s                          <- match opPowerSet expr
        TypeSet{}                  <- typeOf s
        "ExplicitVarSizeWithFlags" <- representationOf s
        [flags, values]            <- downX1 s
        DomainMatrix index _       <- domainOf values
        let upd val old = lambdaToFunction setPat old val
        return
            ( "Vertical rule for set-comprehension, ExplicitVarSizeWithFlagst representation"
            , \ fresh ->
                let outPats =
                        [ quantifiedVar (fresh `at` fromInteger i) | i <- take setPatNum allNats ]
                    val = AbstractLiteral $ AbsLitSet
                        [ [essence| &values[&j] |] | (_,j) <- outPats ]
                in
                    Comprehension (upd val body) $ concat
                        [ gocBefore
                        , concat
                            [ [ Generator (GenDomainNoRepr pat index)
                              , Condition [essence| &flags[&patX] |]
                              ]
                            | (pat,patX) <- take 1 outPats
                            ]
                        , concat
                            [ [ Generator (GenDomainNoRepr pat index)
                              , Condition [essence| &patX > &beforeX |]
                              , Condition [essence| &flags[&patX] |]
                              ]
                            | ((_, beforeX), (pat, patX)) <- zip outPats (tail outPats)
                            ]
                        , transformBi (upd val) gocAfter
                        ]
            )
    theRule _ = na "rule_PowerSet_Comprehension"
