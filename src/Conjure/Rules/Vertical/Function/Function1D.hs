{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Function.Function1D where

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
rule_Comprehension = "function-comprehension{Function1D}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, func), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet,opToMSet,opToRelation] expr)
            _ -> na "rule_Comprehension"
        "Function1D"               <- representationOf func
        TypeFunction tyFr _        <- typeOf func
        DomainFunction _ _ index _ <- domainOf func
        [values]                   <- downX1 func
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over a function, Function1D representation"
            , \ fresh ->
                let
                    (jPat, j) = quantifiedVar (fresh `at` 0)
                    valuesIndexed =
                        if tyFr == TypeBool
                            then [essence| (&j, &values[toInt(&j)]) |]      -- turn the second component into a bool
                            else [essence| (&j, &values[      &j ]) |]
                in
                    Comprehension
                       (upd valuesIndexed body)
                       $  gofBefore
                       ++ [Generator (GenDomainNoRepr jPat (forgetRepr "" index))]
                       ++ transformBi (upd valuesIndexed) gofAfter
               )
    theRule _ = na "rule_Comprehension"


rule_Image :: Rule
rule_Image = "function-image{Function1D}" `namedRule` theRule where
    theRule [essence| image(&func,&x) |] = do
        "Function1D"        <- representationOf func
        TypeFunction tyFr _ <- typeOf func
        [values]            <- downX1 func
        return
            ( "Function image, Function1D representation"
            , const $ if tyFr == TypeBool
                then [essence| &values[toInt(&x)] |]
                else [essence| &values[      &x ] |]
            )
    theRule _ = na "rule_Image"
