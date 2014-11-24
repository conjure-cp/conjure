{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Function.Function1DPartial where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Language.Domain
import Conjure.Language.DomainOf
import Conjure.Language.Pretty
import Conjure.Language.TypeOf
import Conjure.Language.Lenses
import Conjure.Language.TH

import Conjure.Rules.Definition ( Rule(..), namedRule, hasRepresentation, isAtomic, representationOf, matchFirst )

import Conjure.Representations ( downX1 )


rule_Function_Comprehension_Function1DPartial :: Rule
rule_Function_Comprehension_Function1DPartial = "function-comprehension{Function1DPartial}"
                                     `namedRule` theRule where
    theRule (Comprehension body gensOrFilters) = do
        (gofBefore, (pat, iPat, expr), gofAfter) <- matchFirst gensOrFilters $ \ gof -> case gof of
            Generator (GenInExpr pat@(Single iPat) expr) -> return (pat, iPat, expr)
            _ -> fail "No match."        
        let func             =  matchDef opToSet expr
        "Function1DPartial"  <- representationOf func
        TypeFunction{}       <- typeOf func
        [flags,values]       <- downX1 func
        DomainMatrix index _ <- domainOf values
        let i = Reference iPat Nothing
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over a function, Function1DPartial representation"
            , const $ let val = [essence| (&i, &values[&i]) |] in
                Comprehension (upd val body)
                    $  gofBefore
                    ++ [ Generator (GenDomain pat index)
                      , Filter [essence| &flags[&i] |]
                      ]
                    ++ transformBi (upd val) gofAfter
            )
    theRule _ = fail "No match."


rule_Function_Image_Function1DPartial :: Rule
rule_Function_Image_Function1DPartial = "function-image{Function1DPartial}"
                                 `namedRule` theRule where
    theRule [essence| image(&f,&x) |] = do
        "Function1DPartial" <- representationOf f
        [flags,values]      <- downX1 f
        return ( "Function image, Function1DPartial representation"
               , const [essence| { &values[&x]
                                 @ such that &flags[&x]
                                 }
                       |]
               )
    theRule _ = fail "No match."


rule_Function_InDefined_Function1DPartial :: Rule
rule_Function_InDefined_Function1DPartial = "function-in-defined{Function1DPartial}"
                                 `namedRule` theRule where
    theRule [essence| &x in defined(&f) |] = do
        "Function1DPartial" <- representationOf f
        [flags,_values]     <- downX1 f
        return ( "Function in defined, Function1DPartial representation"
               , const [essence| &flags[&x] |]
               )
    theRule _ = fail "No match."
