{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Function.FunctionNDPartial where

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


rule_Image_NotABool :: Rule
rule_Image_NotABool = "function-image{FunctionNDPartial}-not-a-bool" `namedRule` theRule where
    theRule [essence| image(&f,&x) |] = do
        "FunctionNDPartial" <- representationOf f
        TypeFunction _ tyTo <- typeOf f
        case tyTo of
            TypeBool -> na "function ? --> bool"
            _        -> return ()
        [flags,values]      <- downX1 f
        TypeTuple ts        <- typeOf x
        let xArity          =  length ts
        let index m 1     = make opIndexing m                   (make opIndexing x 1)
            index m arity = make opIndexing (index m (arity-1)) (make opIndexing x (fromInt arity))
        let flagsIndexed  = index flags  xArity
        let valuesIndexed = index values xArity

        return ( "Function image, FunctionNDPartial representation, not-a-bool"
               , const [essence| { &valuesIndexed
                                 @ such that &flagsIndexed
                                 } |]
               )
    theRule _ = na "rule_Image_NotABool"


rule_Image_Bool :: Rule
rule_Image_Bool = "function-image{FunctionNDPartial}-bool" `namedRule` theRule where
    theRule p = do
        let
            imageChild ch@[essence| image(&f,&x) |] = do
                "FunctionNDPartial" <- representationOf f
                TypeFunction _ tyTo <- typeOf f
                case tyTo of
                    TypeBool -> do
                        [flags,values]      <- downX1 f
                        TypeTuple ts        <- typeOf x
                        let xArity          =  length ts
                        let index m 1     = make opIndexing m                   (make opIndexing x 1)
                            index m arity = make opIndexing (index m (arity-1)) (make opIndexing x (fromInt arity))
                        let flagsIndexed  = index flags  xArity
                        let valuesIndexed = index values xArity

                        tell $ return flagsIndexed
                        return valuesIndexed
                    _ -> return ch
            imageChild ch = return ch
        (p', flags) <- runWriterT (descendM imageChild p)
        case flags of
            [] -> na "rule_Image_Bool"
            _  -> do
                let flagsCombined = make opAnd $ fromList flags
                return
                    ( "Function image, FunctionNDPartial representation, bool"
                    , const [essence| { &p' @ such that &flagsCombined } |]
                    )


rule_InDefined :: Rule
rule_InDefined = "function-in-defined{FunctionNDPartial}" `namedRule` theRule where
    theRule [essence| &x in defined(&f) |] = do
        "FunctionNDPartial" <- representationOf f
        [flags,_values]     <- downX1 f

        TypeTuple ts        <- typeOf x
        let xArity          =  length ts
        let index m 1     = make opIndexing m                   (make opIndexing x 1)
            index m arity = make opIndexing (index m (arity-1)) (make opIndexing x (fromInt arity))
        let flagsIndexed  = index flags  xArity

        return ( "Function in defined, FunctionNDPartial representation"
               , const flagsIndexed
               )
    theRule _ = na "rule_InDefined"


rule_Comprehension :: Rule
rule_Comprehension = "function-comprehension{FunctionNDPartial}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, func), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDef opToSet expr)
            _ -> na "rule_Comprehension"
        "FunctionNDPartial"           <- representationOf func
        TypeFunction (TypeTuple ts) _ <- typeOf func
        [flags,values]                <- downX1 func
        valuesDom                     <- domainOf values
        let (indexDomain,_)           =  getIndices valuesDom

        let xArity          =  length ts
        let index x m 1     = make opIndexing m                     (make opIndexing x 1)
            index x m arity = make opIndexing (index x m (arity-1)) (make opIndexing x (fromInt arity))
        let flagsIndexed  x = index x flags  xArity
        let valuesIndexed x = index x values xArity

        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over a function, FunctionNDPartial representation"
            , \ fresh ->
                let
                    (jPat, j) = quantifiedVar (fresh `at` 0)
                    val' = valuesIndexed j
                    val  = [essence| (&j, &val') |]
                in
                    Comprehension (upd val body)
                    $  gofBefore
                    ++ [ Generator (GenDomainNoRepr jPat (DomainTuple indexDomain))
                       , Condition (flagsIndexed j)
                       ]
                    ++ transformBi (upd val) gofAfter
            )
    theRule _ = na "rule_Comprehension"
