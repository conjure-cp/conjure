{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Function.FunctionNDPartial where

import Conjure.Rules.Import


rule_Image_NotABool :: Rule
rule_Image_NotABool = "function-image{FunctionNDPartial}-not-a-bool" `namedRule` theRule where
    theRule [essence| image(&f,&x) |] = do
        Function_NDPartial  <- representationOf f
        TypeFunction _ tyTo <- typeOf f
        case tyTo of
            TypeBool -> na "function ? --> bool"
            _        -> return ()
        [flags,values]      <- downX1 f
        toIndex             <- downX1 x
        let
            flagsIndexed  = make opMatrixIndexing flags  toIndex
            valuesIndexed = make opMatrixIndexing values toIndex

        return
            ( "Function image, FunctionNDPartial representation, not-a-bool"
            , return [essence| ?#{ &valuesIndexed
                               @ such that &flagsIndexed
                               } |]
            )
    theRule _ = na "rule_Image_NotABool"


rule_Image_Bool :: Rule
rule_Image_Bool = "function-image{FunctionNDPartial}-bool" `namedRule` theRule where
    theRule p = do
        let
            imageChild ch@[essence| image(&f,&x) |] = do
                Function_NDPartial  <- representationOf f
                TypeFunction _ tyTo <- typeOf f
                case tyTo of
                    TypeBool -> do
                        [flags,values]      <- downX1 f
                        toIndex             <- downX1 x
                        let flagsIndexed  = make opMatrixIndexing flags  toIndex
                        let valuesIndexed = make opMatrixIndexing values toIndex
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
                    , return [essence| ?#{ &p' @ such that &flagsCombined } |]
                    )


rule_InDefined :: Rule
rule_InDefined = "function-in-defined{FunctionNDPartial}" `namedRule` theRule where
    theRule [essence| &x in defined(&f) |] = do
        Function_NDPartial  <- representationOf f
        [flags,_values]     <- downX1 f
        toIndex             <- downX1 x
        let flagsIndexed  = make opMatrixIndexing flags toIndex
        return
            ( "Function in defined, FunctionNDPartial representation"
            , return flagsIndexed
            )
    theRule _ = na "rule_InDefined"


rule_Comprehension :: Rule
rule_Comprehension = "function-comprehension{FunctionNDPartial}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, func), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet,opToMSet,opToRelation] expr)
            _ -> na "rule_Comprehension"
        Function_NDPartial                    <- representationOf func
        DomainFunction _ _ innerDomainFr _    <- domainOf func
        [flags,values]                        <- downX1 func
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over a function, FunctionNDPartial representation"
            , do
                (jPat, j) <- quantifiedVar
                let kRange = case innerDomainFr of
                        DomainTuple ts  -> map fromInt [1 .. genericLength ts]
                        DomainRecord rs -> map (fromName . fst) rs
                        _ -> bug $ vcat [ "FunctionNDPartial.rule_Comprehension"
                                        , "innerDomainFr:" <+> pretty innerDomainFr
                                        ]
                    toIndex       = [ [essence| &j[&k] |] | k <- kRange ]
                    flagsIndexed  = make opMatrixIndexing flags  toIndex
                    valuesIndexed = make opMatrixIndexing values toIndex
                    val           = [essence| (&j, &valuesIndexed) |]
                return $ Comprehension (upd val body)
                    $  gocBefore
                    ++ [ Generator (GenDomainNoRepr jPat (forgetRepr innerDomainFr))
                       , Condition flagsIndexed
                       ]
                    ++ transformBi (upd val) gocAfter
            )
    theRule _ = na "rule_Comprehension"
