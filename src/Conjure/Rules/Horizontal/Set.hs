{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.Set where

import Conjure.Rules.Import
import Conjure.Process.Sanity ( isInfinite )

rule_Comprehension_Literal :: Rule
rule_Comprehension_Literal = "set-comprehension-literal" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet, opToMSet] expr)
            Generator (GenInExpr pat@AbsPatSet{} expr) -> return (pat, matchDefs [opToSet, opToMSet] expr)
            _ -> na "rule_Comprehension_Literal"
        (TypeSet tau, elems) <- match setLiteral expr
        let outLiteral = make matrixLiteral
                            (TypeMatrix (TypeInt TagInt) tau)
                            (DomainInt TagInt [RangeBounded 1 (fromInt (genericLength elems))])
                            elems
        return
            ( "Comprehension on set literals"
            , return $ Comprehension body
                     $  gocBefore
                     ++ [Generator (GenInExpr pat outLiteral)]
                     ++ gocAfter
            )
    theRule _ = na "rule_Comprehension_Literal"


rule_Eq :: Rule
rule_Eq = "set-eq" `namedRule` theRule where
    theRule p = do
        (x,y)     <- match opEq p
        TypeSet{} <- typeOf x
        TypeSet{} <- typeOf y
        return
            ( "Horizontal rule for set equality"
            , return $ make opAnd $ fromList
                [ make opSubsetEq x y
                , make opSubsetEq y x
                ]
            )


rule_Neq :: Rule
rule_Neq = "set-neq" `namedRule` theRule where
    theRule [essence| &x != &y |] = do
        TypeSet{} <- typeOf x
        TypeSet{} <- typeOf y
        return
            ( "Horizontal rule for set dis-equality"
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
rule_SubsetEq = "set-subsetEq" `namedRule` theRule where
    theRule p = do
        (x,y)     <- match opSubsetEq p
        TypeSet{} <- typeOf x
        TypeSet{} <- typeOf y
        return
            ( "Horizontal rule for set subsetEq"
            , do
                 (iPat, i) <- quantifiedVar
                 return [essence| forAll &iPat in &x . &i in &y |]
            )


rule_Subset :: Rule
rule_Subset = "set-subset" `namedRule` theRule where
    theRule [essence| &a subset &b |] = do
        TypeSet{} <- typeOf a
        TypeSet{} <- typeOf b
        return
            ( "Horizontal rule for set subset"
            , return [essence| &a subsetEq &b /\ &a != &b |]
            )
    theRule _ = na "rule_Subset"


rule_Supset :: Rule
rule_Supset = "set-supset" `namedRule` theRule where
    theRule [essence| &a supset &b |] = do
        TypeSet{} <- typeOf a
        TypeSet{} <- typeOf b
        return
            ( "Horizontal rule for set supset"
            , return [essence| &b subset &a |]
            )
    theRule _ = na "rule_Supset"


rule_SupsetEq :: Rule
rule_SupsetEq = "set-subsetEq" `namedRule` theRule where
    theRule [essence| &a supsetEq &b |] = do
        TypeSet{} <- typeOf a
        TypeSet{} <- typeOf b
        return
            ( "Horizontal rule for set supsetEq"
            , return [essence| &b subsetEq &a |]
            )
    theRule _ = na "rule_SupsetEq"


rule_Intersect :: Rule
rule_Intersect = "set-intersect" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, iPat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@(Single iPat) expr) ->
                return (pat, iPat, matchDefs [opToSet,opToMSet,opToRelation] expr)
            _ -> na "rule_Intersect"
        (mkModifier, s)    <- match opModifier expr
        (x, y)             <- match opIntersect s
        tx                 <- typeOf x
        case tx of
            TypeSet{}      -> return ()
            TypeMSet{}     -> return ()
            TypeFunction{} -> return ()
            TypeRelation{} -> return ()
            _              -> failDoc "type incompatibility in intersect operator"
        let i = Reference iPat Nothing
        return
            ( "Horizontal rule for set intersection"
            , return $
                Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat (mkModifier x))
                       , Condition [essence| &i in &y |]
                       ]
                    ++ gocAfter
            )
    theRule _ = na "rule_Intersect"


rule_Union :: Rule
rule_Union = "set-union" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, iPat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@(Single iPat) expr) -> return (pat, iPat, matchDef opToSet expr)
            _ -> na "rule_Union"
        (mkModifier, s)    <- match opModifier expr
        (x, y)             <- match opUnion s
        tx                 <- typeOf x
        case tx of
            TypeSet{}      -> return ()
            TypeMSet{}     -> return ()
            TypeFunction{} -> return ()
            TypeRelation{} -> return ()
            _              -> failDoc "type incompatibility in union operator"
        let i = Reference iPat Nothing
        return
            ( "Horizontal rule for set union"
            , return $ make opFlatten $ AbstractLiteral $ AbsLitMatrix
                (DomainInt TagInt [RangeBounded 1 2])
                [ Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat (mkModifier x)) ]
                    ++ gocAfter
                , Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat (mkModifier y))
                       , Condition [essence| !(&i in &x) |]
                       ]
                    ++ gocAfter
                ]
            )
    theRule _ = na "rule_Union"


rule_Difference :: Rule
rule_Difference = "set-difference" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, iPat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@(Single iPat) expr) -> return (pat, iPat, expr)
            _ -> na "rule_Difference"
        (mkModifier, s)    <- match opModifier expr
        (x, y)             <- match opMinus s
        tx                 <- typeOf x
        case tx of
            TypeSet{}      -> return ()
            TypeMSet{}     -> return ()
            TypeFunction{} -> return ()
            TypeRelation{} -> return ()
            _              -> failDoc "type incompatibility in difference operator"
        let i = Reference iPat Nothing
        return
            ( "Horizontal rule for set difference"
            , return $
                Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat (mkModifier x))
                       , Condition [essence| !(&i in &y) |]
                       ]
                    ++ gocAfter
            )
    theRule _ = na "rule_Difference"


rule_PowerSet_Difference :: Rule
rule_PowerSet_Difference = "set-powerSet-difference" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat expr) -> return (pat, expr)
            _ -> na "rule_PowerSet_Difference"
        setExpr            <- match opPowerSet expr
        (x, y)             <- match opMinus setExpr
        let patAsExpr = patternToExpr pat
        return
            ( "Horizontal rule for set powerSet difference"
            , return $
                Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat (make opPowerSet x))
                       , Condition [essence| !(&patAsExpr subsetEq &y) |]
                       ]
                    ++ gocAfter
            )
    theRule _ = na "rule_PowerSet_Difference"


rule_PowerSet_Comprehension :: Rule
rule_PowerSet_Comprehension = "set-powerSet-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (patName, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr (Single patName) expr) -> return (patName, expr)
            _ -> na "rule_PowerSet_Comprehension"
        s                             <- match opPowerSet expr
        sDom                          <- domainOf s
        let sDom' =
                -- only keep the maxsize attribute
                case sDom of
                    DomainSet () (SetAttr sAttr) sInner ->
                        let
                            sAttr' =
                                case sAttr of
                                    SizeAttr_None -> SizeAttr_None
                                    SizeAttr_Size x -> SizeAttr_MaxSize x
                                    SizeAttr_MinSize _ -> SizeAttr_None
                                    SizeAttr_MaxSize x -> SizeAttr_MaxSize x
                                    SizeAttr_MinMaxSize _ x -> SizeAttr_MaxSize x
                        in
                            DomainSet () (SetAttr sAttr') sInner
                    _ -> sDom
        let pat = Single patName
        let patAsExpr = Reference patName Nothing
        return
            ( "Horizontal rule for set-comprehension over powerSet"
            , return $
                Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenDomainNoRepr pat sDom')
                       , Condition [essence| &patAsExpr subsetEq &s |]
                       ]
                    ++ gocAfter
            )
    theRule _ = na "rule_PowerSet_Comprehension"


rule_MaxMin :: Rule
rule_MaxMin = "set-max-min" `namedRule` theRule where
    theRule [essence| max(&s) |] = do
        TypeSet (TypeInt _) <- typeOf s
        return
            ( "Horizontal rule for set max"
            , case () of
                _ | Just (_, xs) <- match setLiteral s, length xs > 0 -> return $ make opMax $ fromList xs
                _ -> do
                    (iPat, i) <- quantifiedVar
                    return [essence| max([&i | &iPat <- &s]) |]
            )
    theRule [essence| min(&s) |] = do
        TypeSet (TypeInt _) <- typeOf s
        return
            ( "Horizontal rule for set min"
            , case () of
                _ | Just (_, xs) <- match setLiteral s, length xs > 0 -> return $ make opMin $ fromList xs
                _ -> do
                    (iPat, i) <- quantifiedVar
                    return [essence| min([&i | &iPat <- &s]) |]
            )
    theRule _ = na "rule_MaxMin"


-- x in s ~~> or([ x = i | i in s ])
rule_In :: Rule
rule_In = "set-in" `namedRule` theRule where
    theRule p = do
        (x,s)     <- match opIn p
        TypeSet{} <- typeOf s
        -- do not apply this rule to quantified variables
        -- or else we might miss the opportunity to apply a more specific vertical rule
        if referenceToComprehensionVar s
            then na "rule_In"
            else return ()
        return
            ( "Horizontal rule for set-in."
            , do
                 (iPat, i) <- quantifiedVar
                 return [essence| exists &iPat in &s . &i = &x |]
            )


rule_Card :: Rule
rule_Card = "set-card" `namedRule` theRule where
    theRule p = do
        s         <- match opTwoBars p
        case s of
            Domain{} -> na "rule_Card"
            _        -> return ()
        TypeSet{} <- typeOf s
        return
            ( "Horizontal rule for set cardinality."
            , do
                mdom <- runMaybeT $ domainOf s
                case mdom of
                    Just (DomainSet _ (SetAttr (SizeAttr_Size n)) _) -> return n
                    _ -> do
                        (iPat, _) <- quantifiedVar
                        return [essence| sum &iPat in &s . 1 |]
            )


rule_CardViaFreq :: Rule
rule_CardViaFreq = "set-card-via-freq" `namedRule` theRule where
    theRule [essence| freq(toMSet(&s),&x) |] = do
        case s of
            Domain{} -> na "rule_CardViaFreq"
            _        -> return ()
        TypeSet{} <- typeOf s
        return
            ( "Horizontal rule for set cardinality."
            , return [essence| toInt(&x in &s) |]
            )
    theRule _ = na "rule_CardViaFreq"


rule_Param_MinOfSet :: Rule
rule_Param_MinOfSet = "param-min-of-set" `namedRule` theRule where
    theRule [essence| min(&s) |] = do
        TypeSet (TypeInt _) <- typeOf s
        unless (categoryOf s == CatParameter) $ na "rule_Param_MinOfSet"
        isDomainExpr s
        DomainSet _ _ inner <- domainOf s
        case inner of
            DomainInt _ rs | isInfinite rs -> na "rule_Param_MaxOfSet"
            _ -> return ()
        return
            ( "min of a parameter set"
            , case inner of
                DomainInt _ [RangeBounded l _] -> return l
                _ -> do
                    (iPat, i) <- quantifiedVar
                    return [essence| min([ &i | &iPat : &inner ]) |]
            )
    theRule _ = na "rule_Param_MinOfSet"


rule_Param_MaxOfSet :: Rule
rule_Param_MaxOfSet = "param-max-of-set" `namedRule` theRule where
    theRule [essence| max(&s) |] = do
        TypeSet (TypeInt _) <- typeOf s
        unless (categoryOf s == CatParameter) $ na "rule_Param_MaxOfSet"
        isDomainExpr s
        DomainSet _ _ inner <- domainOf s
        case inner of
            DomainInt _ rs | isInfinite rs -> na "rule_Param_MaxOfSet"
            _ -> return ()
        return
            ( "max of a parameter set"
            , case inner of
                DomainInt _ [RangeBounded _ u] -> return u
                _ -> do
                    (iPat, i) <- quantifiedVar
                    return [essence| max([ &i | &iPat : &inner ]) |]
            )
    theRule _ = na "rule_Param_MaxOfSet"


rule_Param_Card :: Rule
rule_Param_Card = "param-card-of-set" `namedRule` theRule where
    theRule [essence| |&s| |] = do
        TypeSet (TypeInt _) <- typeOf s
        unless (categoryOf s == CatParameter) $ na "rule_Param_Card"
        DomainSet _ (SetAttr (SizeAttr_Size n)) _ <- domainOf s
        return
            ( "cardinality of a parameter set"
            , return n
            )
    theRule _ = na "rule_Param_Card"
