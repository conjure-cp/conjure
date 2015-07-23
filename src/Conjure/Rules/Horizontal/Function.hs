{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.Function where

import Conjure.Rules.Import


rule_Comprehension_Literal :: Rule
rule_Comprehension_Literal = "function-comprehension-literal" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet,opToMSet,opToRelation] expr)
            _ -> na "rule_Comprehension_Literal"
        (TypeFunction fr to, elems) <- match functionLiteral expr
        let outLiteral = make matrixLiteral
                            (TypeMatrix TypeInt (TypeTuple [fr,to]))
                            (DomainInt [RangeBounded 1 (fromInt (genericLength elems))])
                            [ AbstractLiteral (AbsLitTuple [a,b])
                            | (a,b) <- elems
                            ]
        let upd val old = lambdaToFunction pat old val
        return
            ( "Comprehension on function literals"
            , do
                (iPat, i) <- quantifiedVar
                return $ Comprehension (upd i body)
                    $  gocBefore
                    ++ [Generator (GenInExpr iPat outLiteral)]
                    ++ transformBi (upd i) gocAfter
            )
    theRule _ = na "rule_Comprehension_Literal"


rule_Eq :: Rule
rule_Eq = "function-eq" `namedRule` theRule where
    theRule p = do
        (x,y)          <- match opEq p
        TypeFunction{} <- typeOf x
        TypeFunction{} <- typeOf y
        return
            ( "Horizontal rule for function equality"
            , do
                (iPat, i) <- quantifiedVar
                return [essence|
                            (forAll &iPat in &x . &y(&i[1]) = &i[2])
                                /\
                            (forAll &iPat in &y . &x(&i[1]) = &i[2])
                                /\
                            defined(&x) = defined(&y)
                       |]
            )


rule_Neq :: Rule
rule_Neq = "function-neq" `namedRule` theRule where
    theRule [essence| &x != &y |] = do
        TypeFunction{} <- typeOf x
        TypeFunction{} <- typeOf y
        return
            ( "Horizontal rule for function dis-equality"
            , do
                (iPat, i) <- quantifiedVar
                return [essence|
                            (exists &iPat in &x . !(&i in &y))
                            \/
                            (exists &iPat in &y . !(&i in &x))
                       |]
            )
    theRule _ = na "rule_Neq"


rule_SubsetEq :: Rule
rule_SubsetEq = "function-subsetEq" `namedRule` theRule where
    theRule p = do
        (x,y)          <- match opSubsetEq p
        TypeFunction{} <- typeOf x
        TypeFunction{} <- typeOf y
        return
            ( "Horizontal rule for function subsetEq"
            , do
                (iPat, i) <- quantifiedVar
                return [essence|
                            (forAll &iPat in &x . &y(&i[1]) = &i[2])
                                /\
                            defined(&x) subsetEq defined(&y)
                       |]
            )


rule_Subset :: Rule
rule_Subset = "function-subset" `namedRule` theRule where
    theRule [essence| &a subset &b |] = do
        TypeFunction{} <- typeOf a
        TypeFunction{} <- typeOf b
        return
            ( "Horizontal rule for function subset"
            , return [essence| &a subsetEq &b /\ &a != &b |]
            )
    theRule _ = na "rule_Subset"


rule_Supset :: Rule
rule_Supset = "function-supset" `namedRule` theRule where
    theRule [essence| &a supset &b |] = do
        TypeFunction{} <- typeOf a
        TypeFunction{} <- typeOf b
        return
            ( "Horizontal rule for function supset"
            , return [essence| &b subset &a |]
            )
    theRule _ = na "rule_Supset"


rule_SupsetEq :: Rule
rule_SupsetEq = "function-subsetEq" `namedRule` theRule where
    theRule [essence| &a supsetEq &b |] = do
        TypeFunction{} <- typeOf a
        TypeFunction{} <- typeOf b
        return
            ( "Horizontal rule for function supsetEq"
            , return [essence| &b subsetEq &a |]
            )
    theRule _ = na "rule_SupsetEq"


rule_DotLt :: Rule
rule_DotLt = "function-DotLt" `namedRule` theRule where
    theRule p = do
        (a,b)          <- match opDotLt p
        TypeFunction{} <- typeOf a
        TypeFunction{} <- typeOf b
        sameRepresentation a b
        ma <- tupleLitIfNeeded <$> downX1 a
        mb <- tupleLitIfNeeded <$> downX1 b
        return
            ( "Horizontal rule for function .<" <+> pretty (make opDotLt ma mb)
            , return $ make opDotLt ma mb
            )


rule_DotLeq :: Rule
rule_DotLeq = "function-DotLeq" `namedRule` theRule where
    theRule p = do
        (a,b)          <- match opDotLeq p
        TypeFunction{} <- typeOf a
        TypeFunction{} <- typeOf b
        sameRepresentation a b
        ma <- tupleLitIfNeeded <$> downX1 a
        mb <- tupleLitIfNeeded <$> downX1 b
        return
            ( "Horizontal rule for function .<=" <+> pretty (make opDotLeq ma mb)
            , return $ make opDotLeq ma mb
            )


rule_Inverse :: Rule
rule_Inverse = "function-inverse" `namedRule` theRule where
    theRule [essence| inverse(&a, &b) |] = do
        TypeFunction{} <- typeOf a
        TypeFunction{} <- typeOf b
        return
            ( "Horizontal rule for function inverse"
            , do
                (iPat, i) <- quantifiedVar
                return
                    [essence|
                        (forAll &iPat in &a . &b(&i[2]) = &i[1])
                            /\
                        (forAll &iPat in &b . &a(&i[2]) = &i[1])
                    |]
            )
    theRule _ = na "rule_Inverse"


rule_Comprehension_PreImage :: Rule
rule_Comprehension_PreImage = "function-preImage" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_PreImage"
        (func, img) <- match opPreImage expr
        TypeFunction{} <- typeOf func
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over the preImage of a function"
            , do
                (jPat, j) <- quantifiedVar
                let val = [essence| &j[1] |]
                return $
                    Comprehension
                        (upd val body)
                        $  gocBefore
                        ++ [ Generator (GenInExpr jPat func)
                           , Condition [essence| &j[2] = &img |]
                           ]
                        ++ transformBi (upd val) gocAfter
            )
    theRule _ = na "rule_Comprehension_PreImage"


rule_Card :: Rule
rule_Card = "function-cardinality" `namedRule` theRule where
    theRule [essence| |&f| |] = do
        TypeFunction{} <- typeOf f
        return
            ( "Function cardinality"
            , do
                dom <- domainOf f
                case dom of
                    DomainFunction _ (FunctionAttr (SizeAttr_Size n) _ _) _ _
                        -> return n
                    DomainFunction _ (FunctionAttr _ _ jectivity) _ innerTo
                        | jectivity `elem` [JectivityAttr_Surjective, JectivityAttr_Bijective]
                        -> domainSizeOf innerTo
                    DomainFunction _ (FunctionAttr _ PartialityAttr_Total _) innerFr _
                        -> domainSizeOf innerFr
                    _ -> return [essence| |toSet(&f)| |]
            )
    theRule _ = na "rule_Card"


rule_Comprehension_Defined :: Rule
rule_Comprehension_Defined = "function-defined" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Defined"
        func <- match opDefined expr
        DomainFunction _ _ domFr _domTo <- domainOf func
        unless (null [ () | DomainAny{} <- universe domFr ]) $ na "Cannot compute the domain of defined(f)"
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over defined(f)"
            , do
                (auxName, aux) <- auxiliaryVar
                (jPat, j) <- quantifiedVar
                (kPat, k) <- quantifiedVar
                (lPat, l) <- quantifiedVar
                let k1 = [essence| &k[1] |]
                let l1 = [essence| &l[1] |]
                return $ WithLocals
                    (Comprehension
                        (upd j body)
                        $  gocBefore
                        ++ [ Generator (GenInExpr jPat aux) ]
                        ++ transformBi (upd j) gocAfter)
                    (Left [ Declaration (FindOrGiven LocalFind auxName (DomainSet def def (forgetRepr domFr)))
                          , SuchThat
                              [ make opAnd $ Comprehension
                                  [essence| &k1 in &aux |]
                                  [ Generator (GenInExpr kPat func) ]
                              , make opAnd $
                                  Comprehension
                                      (make opOr $ Comprehension
                                          [essence| &l1 = &k |]
                                          [ Generator (GenInExpr lPat func) ]
                                      )
                                      [ Generator (GenInExpr kPat aux) ]
                              ]
                          ])
            )
    theRule _ = na "rule_Comprehension_Defined"


rule_Comprehension_Range :: Rule
rule_Comprehension_Range = "function-range" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Range"
        func <- match opRange expr
        DomainFunction _ _ _domFr domTo <- domainOf func
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over range(f)"
            , do
                (auxName, aux) <- auxiliaryVar
                (jPat, j) <- quantifiedVar
                (kPat, k) <- quantifiedVar
                (lPat, l) <- quantifiedVar
                let k2 = [essence| &k[2] |]
                let l2 = [essence| &l[2] |]
                return $ WithLocals
                    (Comprehension
                        (upd j body)
                        $  gocBefore
                        ++ [ Generator (GenInExpr jPat aux) ]
                        ++ transformBi (upd j) gocAfter)
                    (Left [ Declaration (FindOrGiven LocalFind auxName (DomainSet def def (forgetRepr domTo)))
                          , SuchThat
                              [ make opAnd $ Comprehension
                                  [essence| &k2 in &aux |]
                                  [ Generator (GenInExpr kPat func) ]
                              , make opAnd $
                                  Comprehension
                                      (make opOr $ Comprehension
                                          [essence| &l2 = &k |]
                                          [ Generator (GenInExpr lPat func) ]
                                      )
                                      [ Generator (GenInExpr kPat aux) ]
                              ]
                          ])
            )
    theRule _ = na "rule_Comprehension_Range"


rule_Comprehension_Defined_Size :: Rule
rule_Comprehension_Defined_Size = "function-defined-size" `namedRule` theRule where
    theRule [essence| size(defined(&func), &n) |] = do
        DomainFunction _ _ domFr _domTo <- domainOf func
        return
            ( "size(defined(func), n)"
            , do
                (auxName, aux) <- auxiliaryVar
                (kPat, k) <- quantifiedVar
                (lPat, l) <- quantifiedVar
                let k1 = [essence| &k[1] |]
                let l1 = [essence| &l[1] |]
                return $ WithLocals
                    (fromBool True)
                    (Left [ Declaration (FindOrGiven LocalFind auxName
                                  (DomainSet def (SetAttr (SizeAttr_Size n)) (forgetRepr domFr)))
                          , SuchThat
                              [ make opAnd $ Comprehension
                                  [essence| &k1 in &aux |]
                                  [ Generator (GenInExpr kPat func) ]
                              , make opAnd $
                                  Comprehension
                                      (make opOr $ Comprehension
                                          [essence| &l1 = &k |]
                                          [ Generator (GenInExpr lPat func) ]
                                      )
                                      [ Generator (GenInExpr kPat aux) ]
                              ]
                          ])
            )
    theRule _ = na "rule_Comprehension_Defined_Size"


rule_Comprehension_Range_Size :: Rule
rule_Comprehension_Range_Size = "function-range-size" `namedRule` theRule where
    theRule [essence| size(range(&func), &n) |] = do
        DomainFunction _ _ _domFr domTo <- domainOf func
        return
            ( "size(range(func), n)"
            , do
                (auxName, aux) <- auxiliaryVar
                (kPat, k) <- quantifiedVar
                (lPat, l) <- quantifiedVar
                let k2 = [essence| &k[2] |]
                let l2 = [essence| &l[2] |]
                return $ WithLocals
                    (fromBool True)
                    (Left [ Declaration (FindOrGiven LocalFind auxName
                                  (DomainSet def (SetAttr (SizeAttr_Size n)) (forgetRepr domTo)))
                          , SuchThat
                              [ make opAnd $ Comprehension
                                  [essence| &k2 in &aux |]
                                  [ Generator (GenInExpr kPat func) ]
                              , make opAnd $
                                  Comprehension
                                      (make opOr $ Comprehension
                                          [essence| &l2 = &k |]
                                          [ Generator (GenInExpr lPat func) ]
                                      )
                                      [ Generator (GenInExpr kPat aux) ]
                              ]
                          ])
            )
    theRule _ = na "rule_Comprehension_Range_Size"


rule_In :: Rule
rule_In = "function-in" `namedRule` theRule where
    theRule [essence| &x in &f |] = do
        TypeFunction{} <- typeOf f
        return
            ( "Function membership to function image."
            , return [essence| &f(&x[1]) = &x[2] |]
            )
    theRule _ = na "rule_In"


rule_Restrict_Image :: Rule
rule_Restrict_Image = "function-restrict-image" `namedRule` theRule where
    theRule p = do
        (func', arg) <- match opImage p
        (func , dom) <- match opRestrict func'
        TypeFunction{} <- typeOf func
        return
            ( "Function image on a restricted function."
            , do
                (iPat, i) <- quantifiedVar
                let bob = [essence| exists &iPat : &dom . &i = &arg |]
                return $ WithLocals (make opImage func arg) (Right [bob])
            )


rule_Restrict_Comprehension :: Rule
rule_Restrict_Comprehension = "function-restrict-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (iPat, iPatName, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr iPat@(Single iPatName) expr) -> return (iPat, iPatName, expr)
            _ -> na "rule_Comprehension_PreImage"
        (func, dom) <- match opRestrict expr
        TypeFunction{} <- typeOf func
        return
            ( "Mapping over restrict(func, dom)"
            , do
                (jPat, j) <- quantifiedVar
                let i = Reference iPatName Nothing
                return $ Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr iPat func)
                       , Condition [essence| exists &jPat : &dom . &j = &i[1] |]
                       ]
                    ++ gocAfter
            )
    theRule _ = na "rule_Restrict_Comprehension"


-- | image(f,x) can be nasty for non-total functions.
--   1.   if f is a total function, it can readily be replaced by a set expression.
--   2.1. if f isn't total, and if the return type is right, it will always end up as a generator for a comprehension.
--      a vertical rule is needed for such cases.
--   2.2. if the return type is not "right", i.e. it is a bool or an int, i.e. sth we cannot quantify over,
--        the vertical rule is harder.

rule_Image_Bool :: Rule
rule_Image_Bool = "function-image-bool" `namedRule` theRule where
    theRule p = do
        (func, arg) <- match opImage p
        case match opRestrict func of
            Nothing -> return ()
            Just{}  -> na "rule_Image_Bool"         -- do not use this rule for restricted functions
        TypeFunction _ TypeBool <- typeOf func
        return
            ( "Function image, bool."
            , do
                (iPat, i) <- quantifiedVar
                return $ make opOr $ Comprehension [essence| &i[2] |]
                        [ Generator (GenInExpr iPat func)
                        , Condition [essence| &i[1] = &arg |]
                        ]
            )


rule_Image_BoolMatrixIndexed :: Rule
rule_Image_BoolMatrixIndexed = "function-image-BoolMatrixIndexed" `namedRule` theRule where
    theRule p = do
        (matrix, indices)                      <- match opMatrixIndexing p
        (func, arg)                            <- match opImage matrix
        TypeFunction _ (TypeMatrix _ TypeBool) <- typeOf func
        return
            ( "Function image, matrix of int."
            , do
                (iPat, i) <- quantifiedVar
                let i2 = make opMatrixIndexing [essence| &i[2] |] indices
                return $ make opOr $ Comprehension i2
                        [ Generator (GenInExpr iPat func)
                        , Condition [essence| &i[1] = &arg |]
                        ]
            )


rule_Image_Int :: Rule
rule_Image_Int = "function-image-int" `namedRule` theRule where
    theRule p = do
        (func, arg) <- match opImage p
        case match opRestrict func of
            Nothing -> return ()
            Just{}  -> na "rule_Image_Int"          -- do not use this rule for restricted functions
        TypeFunction _ TypeInt <- typeOf func
        return
            ( "Function image, int."
            , do
                (iPat, i) <- quantifiedVar
                let val = make opSum $ Comprehension [essence| &i[2] |]
                        [ Generator (GenInExpr iPat func)
                        , Condition [essence| &i[1] = &arg |]
                        ]
                let isDefined = [essence| &arg in defined(&func) |]
                return $ WithLocals val (Right [isDefined])
            )


rule_Image_IntMatrixIndexed :: Rule
rule_Image_IntMatrixIndexed = "function-image-IntMatrixIndexed" `namedRule` theRule where
    theRule p = do
        (matrix, indices)                      <- match opMatrixIndexing p
        (func, arg)                            <- match opImage matrix
        TypeFunction _ (TypeMatrix _ TypeInt)  <- typeOf func
        return
            ( "Function image, matrix of int."
            , do
                (iPat, i) <- quantifiedVar
                let i2 = make opMatrixIndexing [essence| &i[2] |] indices
                let val = make opSum $ Comprehension i2
                        [ Generator (GenInExpr iPat func)
                        , Condition [essence| &i[1] = &arg |]
                        ]
                let isDefined = [essence| &arg in defined(&func) |]
                return $ WithLocals val (Right [isDefined])
            )


rule_Comprehension_Image :: Rule
rule_Comprehension_Image = "function-image-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Image"
        (mkModifier, expr2) <- match opModifierNoP expr
        (func, arg) <- match opImage expr2
        TypeFunction{} <- typeOf func
        case match opRestrict func of
            Nothing -> return ()
            Just{}  -> na "rule_Image_Bool"         -- do not use this rule for restricted functions
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over the image of a function"
            , do
                (iPat, i) <- quantifiedVar
                (jPat, j) <- quantifiedVar
                return $ Comprehension
                    (upd j body)
                    $  gocBefore
                    ++ [ Generator (GenInExpr iPat (mkModifier func))
                       , Condition [essence| &i[1] = &arg |]
                       , Generator (GenInExpr jPat [essence| &i[2] |])
                       ]
                    ++ transformBi (upd j) gocAfter
            )
    theRule _ = na "rule_Comprehension_Image"


rule_Comprehension_ImageSet :: Rule
rule_Comprehension_ImageSet = "function-imageSet-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_ImageSet"
        (mkModifier, expr2) <- match opModifierNoP expr
        (func, arg) <- match opImageSet expr2
        TypeFunction{} <- typeOf func
        case match opRestrict func of
            Nothing -> return ()
            Just{}  -> na "rule_Comprehension_ImageSet"         -- do not use this rule for restricted functions
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over the imageSet of a function"
            , do
                (iPat, i) <- quantifiedVar
                return $ Comprehension
                    (upd [essence| &i[2] |] body)
                    $  gocBefore
                    ++ [ Generator (GenInExpr iPat (mkModifier func))
                       , Condition [essence| &i[1] = &arg |]
                       ]
                    ++ transformBi (upd [essence| &i[2] |]) gocAfter
            )
    theRule _ = na "rule_Comprehension_ImageSet"


rule_Defined_Intersect :: Rule
rule_Defined_Intersect = "function-Defined-intersect" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, iPat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@(Single iPat) expr) -> return (pat, iPat, expr)
            _ -> na "rule_Defined_Intersect"
        f       <- match opDefined expr
        (x, y)  <- match opIntersect f
        tx      <- typeOf x
        case tx of
            TypeFunction{} -> return ()
            _              -> fail "type incompatibility in intersect operator"
        let i = Reference iPat Nothing
        return
            ( "Horizontal rule for function intersection"
            , return $
                Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat (make opDefined x))
                       , Condition [essence| (&i, image(&x,&i)) in &y |]
                       ]
                    ++ gocAfter
            )
    theRule _ = na "rule_Defined_Intersect"


rule_DefinedOrRange_Union :: Rule
rule_DefinedOrRange_Union = "function-DefinedOrRange-union" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, iPat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@(Single iPat) expr) -> return (pat, iPat, expr)
            _ -> na "rule_DefinedOrRange_Union"
        (mk, f) <- match opDefinedOrRange expr
        (x, y)  <- match opUnion f
        tx      <- typeOf x
        case tx of
            TypeFunction{} -> return ()
            _              -> fail "type incompatibility in union operator"
        let mkx = mk x
        let mky = mk y
        let i = Reference iPat Nothing
        return
            ( "Horizontal rule for function union"
            , return $ make opFlatten $ AbstractLiteral $ AbsLitMatrix
                (DomainInt [RangeBounded 1 2])
                [ Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat mkx) ]
                    ++ gocAfter
                , Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat mky)
                       , Condition [essence| !(&i in &mkx) |]
                       ]
                    ++ gocAfter
                ]
            )
    theRule _ = na "rule_DefinedOrRange_Union"


rule_DefinedOrRange_Difference :: Rule
rule_DefinedOrRange_Difference = "function-DefinedOrRange-difference" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, iPat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@(Single iPat) expr) -> return (pat, iPat, expr)
            _ -> na "rule_DefinedOrRange_Difference"
        (mk, f) <- match opDefinedOrRange expr
        (x, y)  <- match opMinus f
        tx      <- typeOf x
        case tx of
            TypeFunction{} -> return ()
            _              -> fail "type incompatibility in difference operator"
        let mkx = mk x
        let mky = mk y
        let i = Reference iPat Nothing
        return
            ( "Horizontal rule for function difference"
            , return $
                Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat mkx)
                       , Condition [essence| !(&i in &mky) |]
                       ]
                    ++ gocAfter
            )
    theRule _ = na "rule_DefinedOrRange_Difference"


