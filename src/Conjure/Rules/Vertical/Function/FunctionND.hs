{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Function.FunctionND where

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


rule_Image :: Rule
rule_Image = "function-image{FunctionND}" `namedRule` theRule where
    theRule [essence| image(&f,&x) |] = do
        "FunctionND" <- representationOf f
        [values]            <- downX1 f
        TypeTuple ts        <- typeOf x
        let
            toIndex   = [ [essence| &x[&k] |]
                        | k' <- [1 .. length ts]
                        , let k = fromInt k'
                        ]
            valuesIndexed = make opIndexing' values toIndex
        return
            ( "Function image, FunctionND representation"
            , const valuesIndexed
            )
    theRule _ = na "rule_Image"


rule_Comprehension :: Rule
rule_Comprehension = "function-comprehension{FunctionND}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, func), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet,opToMSet,opToRelation] expr)
            _ -> na "rule_Comprehension"
        "FunctionND"                     <- representationOf func
        TypeFunction (TypeTuple ts) _    <- typeOf func
        DomainFunction _ _ indexDomain _ <- domainOf func
        [values]                         <- downX1 func
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over a function, FunctionND representation"
            , \ fresh ->
                let
                    (jPat, j) = quantifiedVar (fresh `at` 0)
                    toIndex   = [ [essence| &j[&k] |]
                                | k' <- [1 .. length ts]
                                , let k = fromInt k'
                                ]
                    valuesIndexed = make opIndexing' values toIndex
                    val  = [essence| (&j, &valuesIndexed) |]
                in
                    Comprehension (upd val body)
                        $  gofBefore
                        ++ [ Generator (GenDomainNoRepr jPat (forgetRepr indexDomain)) ]
                        ++ transformBi (upd val) gofAfter
            )
    theRule _ = na "rule_Comprehension"


rule_Comprehension_Defined :: Rule
rule_Comprehension_Defined = "function-comprehension_defined{FunctionND}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Defined"
        func                             <- match opDefined expr
        "FunctionND"                     <- representationOf func
        DomainFunction _ _ indexDomain _ <- domainOf func
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over a function, FunctionND representation"
            , \ fresh ->
                let
                    (jPat, j) = quantifiedVar (fresh `at` 0)
                    val = j
                in
                    Comprehension (upd val body)
                        $  gofBefore
                        ++ [ Generator (GenDomainNoRepr jPat (forgetRepr indexDomain)) ]
                        ++ transformBi (upd val) gofAfter
            )
    theRule _ = na "rule_Comprehension_Defined"
