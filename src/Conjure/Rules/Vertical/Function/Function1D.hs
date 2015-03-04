{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Function.Function1D where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "function-comprehension{Function1D}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, func), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet,opToMSet,opToRelation] expr)
            _ -> na "rule_Comprehension"
        "Function1D"               <- representationOf func
        TypeFunction{}             <- typeOf func
        DomainFunction _ _ index _ <- domainOf func
        [values]                   <- downX1 func
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over a function, Function1D representation"
            , \ fresh ->
                let
                    (jPat, j) = quantifiedVar (fresh `at` 0)
                    valuesIndexed = [essence| (&j, &values[&j]) |]
                in
                    Comprehension
                       (upd valuesIndexed body)
                       $  gocBefore
                       ++ [Generator (GenDomainNoRepr jPat (forgetRepr index))]
                       ++ transformBi (upd valuesIndexed) gocAfter
               )
    theRule _ = na "rule_Comprehension"


rule_Comprehension_Defined :: Rule
rule_Comprehension_Defined = "function-comprehension_defined{Function1D}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension"
        func                       <- match opDefined expr
        "Function1D"               <- representationOf func
        DomainFunction _ _ index _ <- domainOf func
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over a function, Function1D representation"
            , \ fresh ->
                let
                    (jPat, j) = quantifiedVar (fresh `at` 0)
                    val = j
                in
                    Comprehension (upd val body)
                        $  gocBefore
                        ++ [ Generator (GenDomainNoRepr jPat (forgetRepr index)) ]
                        ++ transformBi (upd val) gocAfter
            )
    theRule _ = na "rule_Comprehension_Defined"


rule_Image :: Rule
rule_Image = "function-image{Function1D}" `namedRule` theRule where
    theRule [essence| image(&func,&x) |] = do
        "Function1D"    <- representationOf func
        TypeFunction {} <- typeOf func
        [values]        <- downX1 func
        return
            ( "Function image, Function1D representation"
            , const [essence| &values[&x] |]
            )
    theRule _ = na "rule_Image"
