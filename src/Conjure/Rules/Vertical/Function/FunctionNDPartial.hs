{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Function.FunctionNDPartial where

import Conjure.Rules.Import


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

        let
            toIndex   = [ [essence| &x[&k] |]
                        | k' <- [1 .. genericLength ts]
                        , let k = fromInt k'
                        ]
            flagsIndexed  = make opMatrixIndexing flags  toIndex
            valuesIndexed = make opMatrixIndexing values toIndex

        return
            ( "Function image, FunctionNDPartial representation, not-a-bool"
            , return [essence| { &valuesIndexed
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
                        let xArity          =  genericLength ts
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
                    , return [essence| { &p' @ such that &flagsCombined } |]
                    )


rule_InDefined :: Rule
rule_InDefined = "function-in-defined{FunctionNDPartial}" `namedRule` theRule where
    theRule [essence| &x in defined(&f) |] = do
        "FunctionNDPartial" <- representationOf f
        [flags,_values]     <- downX1 f
        TypeTuple ts        <- typeOf x

        let
            toIndex   = [ [essence| &x[&k] |]
                        | k' <- [1 .. genericLength ts]
                        , let k = fromInt k'
                        ]
            flagsIndexed = make opMatrixIndexing flags  toIndex

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
        "FunctionNDPartial"                   <- representationOf func
        TypeFunction (TypeTuple ts) _         <- typeOf func
        DomainFunction _ _ (DomainTuple ds) _ <- domainOf func
        [flags,values]                        <- downX1 func
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over a function, FunctionNDPartial representation"
            , do
                (jPat, j) <- quantifiedVar
                let toIndex   = [ [essence| &j[&k] |]
                                | k' <- [1 .. genericLength ts]
                                , let k = fromInt k'
                                ]
                    flagsIndexed  = make opMatrixIndexing flags  toIndex
                    valuesIndexed = make opMatrixIndexing values toIndex
                    val           = [essence| (&j, &valuesIndexed) |]
                return $ Comprehension (upd val body)
                    $  gocBefore
                    ++ [ Generator (GenDomainNoRepr jPat (forgetRepr $ DomainTuple ds))
                       , Condition flagsIndexed
                       ]
                    ++ transformBi (upd val) gocAfter
            )
    theRule _ = na "rule_Comprehension"
