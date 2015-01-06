{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Set.Occurrence where

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
rule_Comprehension = "set-comprehension{Occurrence}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, iPat, s), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@(Single iPat) s) -> return (pat, iPat, s)
            _ -> na "rule_Comprehension"
        TypeSet{}            <- typeOf s
        "Occurrence"         <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        let i = Reference iPat Nothing
        return
            ( "Vertical rule for set-comprehension, Occurrence representation"
            , const $
                Comprehension body
                    $  gofBefore
                    ++ [ Generator (GenDomainNoRepr pat index)
                       , Condition [essence| &m[&i] |]
                       ]
                    ++ gofAfter
            )
    theRule _ = na "rule_Comprehension"


rule_PowerSet_Comprehension :: Rule
rule_PowerSet_Comprehension = "set-powerSet-comprehension{Occurrence}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pats, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr (AbsPatSet pats) expr) -> return (pats, expr)
            _ -> na "rule_PowerSet_Comprehension"
        -- assert pats are Single{}
        patNames <- forM pats $ \ pat -> case pat of Single nm -> return nm
                                                     _ -> na "rule_PowerSet_Comprehension: pat not s Single"
        s                    <- match opPowerSet expr
        TypeSet{}            <- typeOf s
        "Occurrence"         <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        return
            ( "Vertical rule for set-comprehension, Occurrence representation"
            , const $
                Comprehension body $ concat
                    [ gofBefore
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
                    , gofAfter
                    ]
            )
    theRule _ = na "rule_PowerSet_Comprehension"


rule_In :: Rule
rule_In = "set-in{Occurrence}" `namedRule` theRule where
    theRule p = do
        (x, s)       <- match opIn p
        TypeSet{}    <- typeOf s
        "Occurrence" <- representationOf s
        [m]          <- downX1 s
        return ( "Vertical rule for set-in, Occurrence representation"
               , const $ make opIndexing m x
               )
