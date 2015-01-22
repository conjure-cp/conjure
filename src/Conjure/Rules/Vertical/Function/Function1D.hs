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
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDef opToSet expr)
            _ -> na "rule_Comprehension"
        "Function1D"         <- representationOf func
        TypeFunction{}       <- typeOf func
        [values]             <- downX1 func
        DomainMatrix index _ <- domainOf values
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over a function, Function1D representation"
            , \ fresh ->
                let
                    (jPat, j) = quantifiedVar (fresh `at` 0)
                    val = [essence| (&j, &values[&j]) |]
                in
                    Comprehension
                       (upd val body)
                       $  gofBefore
                       ++ [Generator (GenDomainNoRepr jPat index)]
                       ++ transformBi (upd val) gofAfter
               )
    theRule _ = na "rule_Comprehension"


rule_Image :: Rule
rule_Image = "function-image{Function1D}" `namedRule` theRule where
    theRule [essence| image(&f,&x) |] = do
        "Function1D" <- representationOf f
        [values]     <- downX1 f
        return ( "Function image, Function1D representation"
               , const [essence| &values[&x] |]
               )
    theRule _ = na "rule_Image"
