{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Function.FunctionNDPartialDummy where

import Conjure.Rules.Import


rule_Image :: Rule
rule_Image = "function-image{FunctionNDPartialDummy}" `namedRule` theRule where
    theRule [essence| image(&f,&x) |] = do
        Function_NDPartialDummy <- representationOf f
        [values] <- downX1 f
        toIndex <- do
            xTy <- typeOf x
            case xTy of
                TypeTuple{} -> downX1 x
                TypeRecord{} -> downX1 x
                _ -> return [x]
        let valuesIndexed = make opMatrixIndexing values toIndex
        return
            ( "Function image, FunctionND representation"
            , return valuesIndexed
            )
    theRule _ = na "rule_Image"


rule_InDefined :: Rule
rule_InDefined = "function-in-defined{FunctionNDPartialDummy}" `namedRule` theRule where
    theRule [essence| &x in defined(&f) |] = do
        Function_NDPartialDummy <- representationOf f
        [values] <- downX1 f
        toIndex <- do
            xTy <- typeOf x
            case xTy of
                TypeTuple{} -> downX1 x
                TypeRecord{} -> downX1 x
                _ -> return [x]
        DomainFunction _ _ _ innerDomainTo <- domainOf f
        let valuesIndexed = make opMatrixIndexing values toIndex
        let dummy = [essence| min(`&innerDomainTo`) - 1 |]
        return
            ( "Function in defined, FunctionNDPartialDummy representation"
            , return [essence| &valuesIndexed != &dummy |]
            )
    theRule _ = na "rule_InDefined"


rule_Comprehension :: Rule
rule_Comprehension = "function-comprehension{FunctionNDPartialDummy}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, func), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet,opToMSet,opToRelation] expr)
            _ -> na "rule_Comprehension"
        Function_NDPartialDummy <- representationOf func
        DomainFunction _ _ innerDomainFr innerDomainTo <- domainOf func
        [values] <- downX1 func
        let dummy = [essence| min(`&innerDomainTo`) - 1 |]
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over a function, FunctionNDPartialDummy representation"
            , do
                (jPat, j) <- quantifiedVar
                let kRange = case innerDomainFr of
                        DomainTuple ts  -> Just $ map fromInt [1 .. genericLength ts]
                        DomainRecord rs -> Just $ map (fromName . fst) rs
                        _ -> Nothing
                    toIndex       = case kRange of
                                        Just ks -> [ [essence| &j[&k] |] | k <- ks ]
                                        Nothing -> [j]
                    valuesIndexed = make opMatrixIndexing values toIndex
                    val           = [essence| (&j, &valuesIndexed) |]
                return $ Comprehension (upd val body)
                    $  gocBefore
                    ++ [ Generator (GenDomainNoRepr jPat (forgetRepr innerDomainFr))
                       , Condition [essence| &val[2] != &dummy |]
                       ]
                    ++ transformBi (upd val) gocAfter
            )
    theRule _ = na "rule_Comprehension"
