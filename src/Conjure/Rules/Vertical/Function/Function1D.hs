{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Function.Function1D where

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


rule_Function_Comprehension_Function1D :: Rule
rule_Function_Comprehension_Function1D = "function-comprehension{Function1D}"
                                     `namedRule` theRule where
    theRule (Comprehension body gensOrFilters) = do
        (gofBefore, (pat, iPat, expr), gofAfter) <- matchFirst gensOrFilters $ \ gof -> case gof of
            Generator (GenInExpr pat@(Single iPat) expr) -> return (pat, iPat, expr)
            _ -> fail "No match."        
        let func             =  matchDef opToSet expr
        "Function1D"         <- representationOf func
        TypeFunction{}       <- typeOf func
        [values]             <- downX1 func
        DomainMatrix index _ <- domainOf values
        let i = Reference iPat Nothing
        let upd val old = lambdaToFunction pat old val
        return ( "Mapping over a function, Function1D representation"
               , const $ let val = [essence| (&i, &values[&i]) |] in
                   Comprehension
                       (upd val body)
                       $  gofBefore
                       ++ [Generator (GenDomain pat index)]
                       ++ transformBi (upd val) gofAfter
               )
    theRule _ = fail "No match."


rule_Function_Image_Function1D :: Rule
rule_Function_Image_Function1D = "function-image{Function1D}"
                                 `namedRule` theRule where
    theRule [essence| image(&f,&x) |] = do
        "Function1D" <- representationOf f
        [values]     <- downX1 f
        return ( "Function image, Function1D representation"
               , const [essence| &values[&x] |]
               )
    theRule _ = fail "No match."
