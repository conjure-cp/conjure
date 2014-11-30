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
        let xArity          =  length ts
        let index m 1     = make opIndexing m                   (make opIndexing x (fromInt 1))
            index m arity = make opIndexing (index m (arity-1)) (make opIndexing x (fromInt arity))
        let valuesIndexed = index values xArity

        return ( "Function image, FunctionND representation"
               , const valuesIndexed
               )
    theRule _ = na "rule_Image"


rule_Comprehension :: Rule
rule_Comprehension = "function-comprehension{FunctionND}" `namedRule` theRule where
    theRule (Comprehension body gensOrFilters) = do
        (gofBefore, (pat, iPat, expr), gofAfter) <- matchFirst gensOrFilters $ \ gof -> case gof of
            Generator (GenInExpr pat@(Single iPat) expr) -> return (pat, iPat, expr)
            _ -> na "rule_Comprehension"
        let func                      =  matchDef opToSet expr
        "FunctionND"           <- representationOf func
        TypeFunction (TypeTuple ts) _ <- typeOf func
        [values]                      <- downX1 func
        valuesDom                     <- domainOf values
        let (indexDomain,_)           =  getIndices valuesDom

        let xArity          =  length ts
        let index x m 1     = make opIndexing m                     (make opIndexing x (fromInt 1))
            index x m arity = make opIndexing (index x m (arity-1)) (make opIndexing x (fromInt arity))
        let valuesIndexed x = index x values xArity

        let i = Reference iPat Nothing
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over a function, Function1DPartial representation"
            , const $ let val' = valuesIndexed i
                          val  = [essence| (&i, &val') |] in
                Comprehension (upd val body)
                    $  gofBefore
                    ++ [ Generator (GenDomain pat (DomainTuple indexDomain))
                       ]
                    ++ transformBi (upd val) gofAfter
            )
    theRule _ = na "rule_Comprehension"
