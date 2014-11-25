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


rule_Set_Comprehension_Occurrence :: Rule
rule_Set_Comprehension_Occurrence = "set-comprehension{Occurrence}" `namedRule` theRule where
    theRule (Comprehension body gensOrFilters) = do
        (gofBefore, (pat, iPat, s), gofAfter) <- matchFirst gensOrFilters $ \ gof -> case gof of
            Generator (GenInExpr pat@(Single iPat) s) -> return (pat, iPat, s)
            _ -> fail "No match."
        TypeSet{}            <- typeOf s
        "Occurrence"         <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        let i = Reference iPat Nothing
        return ( "Vertical rule for set-comprehension, Occurrence representation"
               , const $
                   Comprehension body
                       $  gofBefore
                       ++ [ Generator (GenDomain pat index)
                          , Filter [essence| &m[&i] |]
                          ]
                       ++ gofAfter
               )
    theRule _ = fail "No match."


rule_Set_In_Occurrence :: Rule
rule_Set_In_Occurrence = "set-in{Occurrence}" `namedRule` theRule where
    theRule p = do
        (x, s)       <- match opIn p
        TypeSet{}    <- typeOf s
        "Occurrence" <- representationOf s
        [m]          <- downX1 s
        return ( "Vertical rule for set-in, Occurrence representation"
               , const $ make opIndexing m x
               )
