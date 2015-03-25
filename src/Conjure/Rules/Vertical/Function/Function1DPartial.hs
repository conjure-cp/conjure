{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Function.Function1DPartial where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "function-comprehension{Function1DPartial}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, func), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet,opToMSet,opToRelation] expr)
            _ -> na "rule_Comprehension"
        "Function1DPartial"        <- representationOf func
        TypeFunction {}            <- typeOf func
        DomainFunction _ _ index _ <- domainOf func
        [flags,values]             <- downX1 func
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over a function, Function1DPartial representation"
            , do
                (jPat, j) <- quantifiedVar
                let valuesIndexed = [essence| (&j, &values[&j]) |]
                let flagsIndexed  = [essence|      &flags [&j]  |]
                return $ Comprehension (upd valuesIndexed body)
                    $  gocBefore
                    ++ [ Generator (GenDomainNoRepr jPat (forgetRepr index))
                       , Condition [essence| &flagsIndexed |]
                       ]
                    ++ transformBi (upd valuesIndexed) gocAfter
            )
    theRule _ = na "rule_Comprehension"


rule_Image_NotABool :: Rule
rule_Image_NotABool = "function-image{Function1DPartial}-not-a-bool" `namedRule` theRule where
    theRule [essence| image(&f,&x) |] = do
        "Function1DPartial" <- representationOf f
        TypeFunction _ tyTo <- typeOf f
        case tyTo of
            TypeBool -> na "function ? --> bool"
            _        -> return ()
        [flags,values] <- downX1 f
        return
            ( "Function image, Function1DPartial representation, not-a-bool"
            , return [essence| { &values[&x]
                               @ such that &flags[&x]
                               }
                             |]
            )
    theRule _ = na "rule_Image_NotABool"


rule_Image_Bool :: Rule
rule_Image_Bool = "function-image{Function1DPartial}-bool" `namedRule` theRule where
    theRule p = do
        let
            imageChild ch@[essence| image(&f,&x) |] = do
                "Function1DPartial" <- representationOf f
                TypeFunction _ tyTo <- typeOf f
                case tyTo of
                    TypeBool -> do
                        [flags,values] <- downX1 f
                        tell $ return [essence| &flags[&x] |]
                        return [essence| &values[&x] |]
                    _ -> return ch
            imageChild ch = return ch
        (p', flags) <- runWriterT (descendM imageChild p)
        case flags of
            [] -> na "rule_Image_Bool"
            _  -> do
                let flagsCombined = make opAnd $ fromList flags
                return
                    ( "Function image, Function1DPartial representation, bool"
                    , return [essence| { &p' @ such that &flagsCombined } |]
                    )


rule_InDefined :: Rule
rule_InDefined = "function-in-defined{Function1DPartial}" `namedRule` theRule where
    theRule [essence| &x in defined(&f) |] = do
        TypeFunction{}      <- typeOf f
        "Function1DPartial" <- representationOf f
        [flags,_values]     <- downX1 f
        return
            ( "Function in defined, Function1DPartial representation"
            , return [essence| &flags[&x] |]
            )
    theRule _ = na "rule_InDefined"
