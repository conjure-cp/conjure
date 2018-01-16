{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Set.Occurrence where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "set-comprehension{Occurrence}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, iPat, s), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@(Single iPat) s) -> return (pat, iPat, matchDefs [opToSet,opToMSet] s)
            _ -> na "rule_Comprehension"
        TypeSet{}            <- typeOf s
        Set_Occurrence       <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        let i = Reference iPat Nothing
        return
            ( "Vertical rule for set-comprehension, Occurrence representation"
            , return $
                Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenDomainNoRepr pat index)
                       , Condition [essence| &m[&i] |]
                       ]
                    ++ gocAfter
            )
    theRule _ = na "rule_Comprehension"


rule_PowerSet_Comprehension :: Rule
rule_PowerSet_Comprehension = "set-powerSet-comprehension{Occurrence}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pats, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr (AbsPatSet pats) expr) -> return (pats, expr)
            _ -> na "rule_PowerSet_Comprehension"
        -- assert pats are Single{}
        patNames <- forM pats $ \ pat -> case pat of Single nm -> return nm
                                                     _ -> na "rule_PowerSet_Comprehension: pat not s Single"
        s                    <- match opPowerSet expr
        TypeSet{}            <- typeOf s
        Set_Occurrence       <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        return
            ( "Vertical rule for set-comprehension, Occurrence representation"
            , return $
                Comprehension body $ concat
                    [ gocBefore
                    , concat
                        [ [ Generator (GenDomainNoRepr (Single pat) index)
                          , Condition [essence| &m[&patX] |]
                          ]
                        | pat <- take 1 patNames
                        , let patX = Reference pat Nothing
                        ]
                    , concat
                        [ [ Generator (GenDomainNoRepr (Single pat) index)
                          , Condition [essence| &patX > &beforeX |]
                          , Condition [essence| &m[&patX] |]
                          ]
                        | (before, pat) <- zip patNames (tail patNames)
                        , let beforeX = Reference before Nothing
                        , let patX = Reference pat Nothing
                        ]
                    , gocAfter
                    ]
            )
    theRule _ = na "rule_PowerSet_Comprehension"


rule_In :: Rule
rule_In = "set-in{Occurrence}" `namedRule` theRule where
    theRule [essence| &x in &s |] = do
        TypeSet{}      <- typeOf s
        Set_Occurrence <- representationOf s
        [m]            <- downX1 s
        return
            ( "Vertical rule for set-in, Occurrence representation"
            , return [essence| &m[&x] |]
            )
    theRule _ = na "rule_In"
