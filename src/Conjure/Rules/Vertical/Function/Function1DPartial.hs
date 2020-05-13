{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Function.Function1DPartial where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "function-comprehension{Function1DPartial}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, func), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet,opToMSet,opToRelation] expr)
            _ -> na "rule_Comprehension"
        Function_1DPartial         <- representationOf func
        TypeFunction{ }            <- typeOf func
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


rule_PowerSet_Comprehension :: Rule
rule_PowerSet_Comprehension = "function-powerSet-comprehension{Function1DPartial}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (setPat, setPatNum, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr setPat@(AbsPatSet pats) expr) -> return (setPat, length pats, expr)
            _ -> na "rule_PowerSet_Comprehension"
        func                       <- match opPowerSet expr
        TypeFunction{}             <- typeOf func
        Function_1DPartial         <- representationOf func
        DomainFunction _ _ index _ <- domainOf func
        [flags,values]             <- downX1 func
        let upd val old = lambdaToFunction setPat old val
        return
            ( "Vertical rule for set-comprehension, Explicit representation"
            , do
                outPats <- replicateM setPatNum quantifiedVar
                let val = AbstractLiteral $ AbsLitSet [ [essence| (&flags[&j], &values[&j]) |] | (_,j) <- outPats ]
                return $ Comprehension (upd val body) $ concat
                        [ gocBefore
                        , concat
                            [ [ Generator (GenDomainNoRepr pat index) ]
                            | (pat,_) <- take 1 outPats
                            ]
                        , concat
                            [ [ Generator (GenDomainNoRepr pat index)
                              , Condition [essence| &patX > &beforeX |]
                              ]
                            | ((_, beforeX), (pat, patX)) <- zip outPats (tail outPats)
                            ]
                        , transformBi (upd val) gocAfter
                        ]
            )
    theRule _ = na "rule_PowerSet_Comprehension"


rule_Image_NotABool :: Rule
rule_Image_NotABool = "function-image{Function1DPartial}-not-a-bool" `namedRule` theRule where
    theRule [essence| image(&f,&x) |] = do
        Function_1DPartial  <- representationOf f
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
                Function_1DPartial  <- representationOf f
                TypeFunction _ tyTo <- typeOf f
                case tyTo of
                    TypeBool -> do
                        [flags,values] <- downX1 f
                        tell $ return [essence| &flags[&x] |]
                        return [essence| { &values[&x] @ such that &flags[&x] } |]
                    _ -> return ch
            imageChild ch = return ch
        topMost <- asks isTopMostZ
        (p', flags) <-
            if topMost
                then -- this term sits at the topmost level, requires special treatment
                    runWriterT (imageChild p)
                else
                    runWriterT (descendM imageChild p)
        case flags of
            [] -> na "rule_Image_Bool"
            _  -> do
                -- let flagsCombined = make opAnd $ fromList flags
                return
                    ( "Function image, Function1DPartial representation, bool"
                    , return p'
                    )


rule_InDefined :: Rule
rule_InDefined = "function-in-defined{Function1DPartial}" `namedRule` theRule where
    theRule [essence| &x in defined(&f) |] = do
        TypeFunction{}      <- typeOf f
        Function_1DPartial  <- representationOf f
        [flags,_values]     <- downX1 f
        return
            ( "Function in defined, Function1DPartial representation"
            , return [essence| &flags[&x] |]
            )
    theRule _ = na "rule_InDefined"


rule_DefinedEqDefined :: Rule
rule_DefinedEqDefined = "set-subsetEq" `namedRule` theRule where
    theRule [essence| defined(&x) = defined(&y) |] = do
        TypeFunction{}     <- typeOf x
        Function_1DPartial <- representationOf x
        [xFlags,_]         <- downX1 x
        TypeFunction{}     <- typeOf y
        Function_1DPartial <- representationOf y
        [yFlags,_]         <- downX1 y
        DomainFunction _ _ xIndex _ <- domainOf x
        DomainFunction _ _ yIndex _ <- domainOf y
        unless (xIndex == yIndex) (na "rule_DefinedEqDefined")
        return
            ( "Horizontal rule for set subsetEq"
            , do
                 (iPat, i) <- quantifiedVar
                 return [essence| forAll &iPat : &xIndex . &xFlags[&i] = &yFlags[&i] |]
            )
    theRule _ = na "rule_DefinedEqDefined"

