{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Function.Function1DPartial where

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
rule_Comprehension = "function-comprehension{Function1DPartial}" `namedRule` theRule where
    theRule (Comprehension body gensOrFilters) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrFilters $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension"
        let func             =  matchDef opToSet expr
        "Function1DPartial"  <- representationOf func
        TypeFunction{}       <- typeOf func
        [flags,values]       <- downX1 func
        DomainMatrix index _ <- domainOf values
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over a function, Function1DPartial representation"
            , \ fresh ->
                let
                    (jPat, j) = quantifiedVar (fresh `at` 0)
                    val = [essence| (&j, &values[&j]) |]
                in
                Comprehension (upd val body)
                    $  gofBefore
                    ++ [ Generator (GenDomain jPat index)
                      , Filter [essence| &flags[&j] |]
                      ]
                    ++ transformBi (upd val) gofAfter
            )
    theRule _ = na "rule_Comprehension"


rule_Image :: Rule
rule_Image = "function-image{Function1DPartial}" `namedRule` theRule where
    theRule [essence| image(&f,&x) |] = do
        "Function1DPartial" <- representationOf f
        [flags,values]      <- downX1 f
        return ( "Function image, Function1DPartial representation"
               , const [essence| { &values[&x]
                                 @ such that &flags[&x]
                                 }
                       |]
               )
    theRule _ = na "rule_Image"


rule_InDefined :: Rule
rule_InDefined = "function-in-defined{Function1DPartial}" `namedRule` theRule where
    theRule [essence| &x in defined(&f) |] = do
        "Function1DPartial" <- representationOf f
        [flags,_values]     <- downX1 f
        return ( "Function in defined, Function1DPartial representation"
               , const [essence| &flags[&x] |]
               )
    theRule _ = na "rule_InDefined"
