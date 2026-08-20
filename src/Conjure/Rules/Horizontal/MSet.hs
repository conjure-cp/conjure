{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.MSet where

import Conjure.Rules.Import


rule_Comprehension_Literal :: Rule
rule_Comprehension_Literal = "mset-comprehension-literal" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Literal"
        (TypeMSet tau, elems) <- match msetLiteral expr
        let outLiteral = make matrixLiteral
                            (TypeMatrix (TypeInt TagInt) tau)
                            (DomainInt TagInt [RangeBounded 1 (fromInt (genericLength elems))])
                            elems
        let upd val old = lambdaToFunction pat old val
        return
            ( "Comprehension on mset literals"
            , do
                 (iPat, i) <- quantifiedVar
                 return $ Comprehension (upd i body)
                         $  gocBefore
                         ++ [Generator (GenInExpr iPat outLiteral)]
                         ++ transformBi (upd i) gocAfter
            )
    theRule _ = na "rule_Comprehension_Literal"



-- freq(toMSet(flatten(m)), arg) ~~> sum([ toInt(arg = i) | i in mset ])
rule_Freq_toMSet_Flatten :: Rule
rule_Freq_toMSet_Flatten = "mset-freq-toMSet_Flatten" `namedRule` theRule where
    theRule p = do
        (mset, arg) <- match opFreq p
        m <- match opToMSet mset >>= match opFlatten
        indexDoms <- indexDomainsOf m
        forM_ indexDoms $ \case
            DomainAny{} -> na "rule_Comprehension_Flatten"
            _ -> return ()
        when (null indexDoms) $ na "rule_Comprehension_Flatten"
        return
            ( "Comprehension on a matrix flatten"
            , do
                (gens, is) <- unzip <$> sequence
                                [ do
                                    (iPat, i) <- quantifiedVar
                                    return (Generator (GenDomainNoRepr iPat d), i)
                                | d <- indexDoms
                                ]
                let mis = make opMatrixIndexing m is
                return $ make opSum $ Comprehension [essence| toInt(&arg = &mis) |] gens
            )


rule_Comprehension_ToSet_Literal :: Rule
rule_Comprehension_ToSet_Literal = "mset-comprehension-toSet-literal" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_ToSet_Literal"
        mset                  <- match opToSet expr
        (TypeMSet tau, elems) <- match msetLiteral mset
        let outLiteralDomain = mkDomainIntB 1 (fromInt $ genericLength elems)
        let outLiteral = make matrixLiteral (TypeMatrix (TypeInt TagInt) tau) outLiteralDomain elems
        let upd val old = lambdaToFunction pat old val
        return
            ( "Comprehension on toSet of mset literals"
            , do
                 (iPat, i) <- quantifiedVar
                 (jPat, j) <- quantifiedVar
                 let iIndexed = [essence| &outLiteral[&i] |]
                 let jIndexed = [essence| &outLiteral[&j] |]
                 return $ Comprehension (upd iIndexed body)
                         $  gocBefore
                         ++ [ Generator (GenDomainNoRepr iPat outLiteralDomain)
                            , Condition [essence|
                                !(exists &jPat : &outLiteralDomain .
                                    (&j < &i) /\ (&iIndexed = &jIndexed))
                                        |]
                            ]
                         ++ transformBi (upd iIndexed) gocAfter
            )
    theRule _ = na "rule_Comprehension_ToSet_Literal"


rule_Comprehension_ToSet :: Rule
rule_Comprehension_ToSet = "mset-comprehension-toSet" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, iPat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@(Single iPat) expr) -> return (pat, iPat, expr)
            _ -> na "rule_Comprehension_ToSet"
        mset <- match opToSet expr
        TypeMSet{} <- typeOf mset
        case tryMatch msetLiteral mset of
            Just{} -> na "rule_Comprehension_ToSet: literal has a more specific rule"
            Nothing -> return ()
        innerDomain <- msetInnerDomain mset
        let i = Reference iPat Nothing
        return
            ( "Comprehension on toSet of a multiset"
            , return $ Comprehension body
                $  gocBefore
                ++ [ Generator (GenDomainNoRepr pat innerDomain)
                   , Condition [essence| freq(&mset, &i) > 0 |]
                   ]
                ++ gocAfter
            )
    theRule _ = na "rule_Comprehension_ToSet"

    msetInnerDomain mset = case tryMatch opUnion mset of
        Just (x, y) -> do
            xInner <- msetInnerDomain x
            yInner <- msetInnerDomain y
            domainUnion xInner yInner
        Nothing -> do
            DomainMSet _ _ inner <- domainOf mset
            return inner


-- A multiset union contains max(freq(x, i), freq(y, i)) copies of each i.
-- Keep all copies from x, then add only the excess copies from y.
rule_Union :: Rule
rule_Union = "mset-union" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, iPat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@(Single iPat) expr) -> return (pat, iPat, expr)
            _ -> na "rule_Union"
        (x, y) <- match opUnion expr
        TypeMSet{} <- typeOf x
        yMaxSize <- msetMaxSize y
        let i = Reference iPat Nothing
        return
            ( "Horizontal rule for multiset union"
            , do
                (jPat, j) <- quantifiedVar
                return $ make opFlatten $ AbstractLiteral $ AbsLitMatrix
                    (DomainInt TagInt [RangeBounded 1 2])
                    [ Comprehension body
                        $  gocBefore
                        ++ [ Generator (GenInExpr pat x) ]
                        ++ gocAfter
                    , Comprehension body
                        $  gocBefore
                        ++ [ Generator (GenInExpr pat [essence| toSet(&y) |])
                           , Generator (GenDomainNoRepr jPat (mkDomainIntB 1 yMaxSize))
                           , Condition [essence| freq(&x, &i) < &j /\ &j <= freq(&y, &i) |]
                           ]
                        ++ gocAfter
                    ]
            )
    theRule _ = na "rule_Union"

    msetMaxSize mset = case tryMatch opUnion mset of
        Just (x, y) -> do
            xMaxSize <- msetMaxSize x
            yMaxSize <- msetMaxSize y
            return [essence| max([&xMaxSize, &yMaxSize]) |]
        Nothing -> do
            DomainMSet _ (MSetAttr sizeAttr _) _ <- domainOf mset
            case sizeAttr of
                SizeAttr_Size size -> return size
                SizeAttr_MaxSize size -> return size
                SizeAttr_MinMaxSize _ size -> return size
                _ -> failDoc "rule_Union maxSize"


rule_Eq :: Rule
rule_Eq = "mset-eq" `namedRule` theRule where
    theRule p = do
        (x,y)      <- match opEq p
        TypeMSet{} <- typeOf x
        TypeMSet{} <- typeOf y
        return
            ( "Horizontal rule for mset equality"
            , do
                 (iPat, i) <- quantifiedVar
                 return
                     [essence|
                         (forAll &iPat in &x . freq(&x,&i) = freq(&y,&i)) /\
                         (forAll &iPat in &y . freq(&x,&i) = freq(&y,&i))
                     |]
            )


rule_Neq :: Rule
rule_Neq = "mset-neq" `namedRule` theRule where
    theRule [essence| &x != &y |] = do
        TypeMSet{} <- typeOf x
        TypeMSet{} <- typeOf y
        return
            ( "Horizontal rule for mset dis-equality"
            , do
                 (iPat, i) <- quantifiedVar
                 return
                     [essence|
                         (exists &iPat in &x . freq(&x,&i) != freq(&y,&i)) \/
                         (exists &iPat in &y . freq(&x,&i) != freq(&y,&i))
                     |]
            )
    theRule _ = na "rule_Neq"


rule_SubsetEq :: Rule
rule_SubsetEq = "mset-subsetEq" `namedRule` theRule where
    theRule p = do
        (x,y)      <- match opSubsetEq p
        TypeMSet{} <- typeOf x
        TypeMSet{} <- typeOf y
        return
            ( "Horizontal rule for mset subsetEq"
            , do
                 (iPat, i) <- quantifiedVar
                 return [essence| forAll &iPat in &x . freq(&x,&i) <= freq(&y,&i) |]
            )


rule_Subset :: Rule
rule_Subset = "mset-subset" `namedRule` theRule where
    theRule [essence| &x subset &y |] = do
        TypeMSet{} <- typeOf x
        TypeMSet{} <- typeOf y
        return
            ( "Horizontal rule for mset subset"
               , do
                    (iPat, i) <- quantifiedVar
                    return
                        [essence|
                            (forAll &iPat in &x . freq(&x,&i) <= freq(&y,&i)) /\
                            (exists &iPat in &x . freq(&x,&i) <  freq(&y,&i))
                        |]
            )
    theRule _ = na "rule_Subset"


rule_Supset :: Rule
rule_Supset = "mset-supset" `namedRule` theRule where
    theRule [essence| &a supset &b |] = do
        TypeMSet{} <- typeOf a
        TypeMSet{} <- typeOf b
        return
            ( "Horizontal rule for mset supset"
            , return [essence| &b subset &a |]
            )
    theRule _ = na "rule_Supset"


rule_SupsetEq :: Rule
rule_SupsetEq = "mset-subsetEq" `namedRule` theRule where
    theRule [essence| &a supsetEq &b |] = do
        TypeMSet{} <- typeOf a
        TypeMSet{} <- typeOf b
        return
            ( "Horizontal rule for mset supsetEq"
            , return [essence| &b subsetEq &a |]
            )
    theRule _ = na "rule_SupsetEq"


rule_MaxMin :: Rule
rule_MaxMin = "mset-max-min" `namedRule` theRule where
    theRule [essence| max(&s) |] = do
        TypeMSet (TypeInt _) <- typeOf s
        return
            ( "Horizontal rule for mset max"
            , case () of
                _ | Just (_, xs) <- match msetLiteral s, length xs > 0 -> return $ make opMax $ fromList xs
                _ -> do
                    (iPat, i) <- quantifiedVar
                    return [essence| max([&i | &iPat <- &s]) |]
            )
    theRule [essence| min(&s) |] = do
        TypeMSet (TypeInt _) <- typeOf s
        return
            ( "Horizontal rule for mset min"
            , case () of
                _ | Just (_, xs) <- match msetLiteral s, length xs > 0 -> return $ make opMin $ fromList xs
                _ -> do
                    (iPat, i) <- quantifiedVar
                    return [essence| min([&i | &iPat <- &s]) |]
            )
    theRule _ = na "rule_MaxMin"


-- freq(x union y, arg) ~~> max([freq(x, arg), freq(y, arg)])
rule_Freq_Union :: Rule
rule_Freq_Union = "mset-freq-union" `namedRule` theRule where
    theRule p = do
        (mset, arg) <- match opFreq p
        (x, y) <- match opUnion mset
        TypeMSet{} <- typeOf x
        return
            ( "Horizontal rule for frequency in a multiset union."
            , return [essence| max([freq(&x, &arg), freq(&y, &arg)]) |]
            )


-- freq(mset,arg) ~~> sum([ toInt(arg = i) | i in mset ])
rule_Freq :: Rule
rule_Freq = "mset-freq" `namedRule` theRule where
    theRule p = do
        (mset, arg) <- match opFreq p
        case match opToMSet mset >>= match opFlatten of
            Nothing -> return ()
            Just{} -> na "There is a better rule for this: rule_Freq_toMSet_Flatten"
        TypeMSet{}  <- typeOf mset
        -- avoid applying this rule when "mset" is of the form "toMSet of set"
        case mset of
            [essence| toMSet(&s) |] -> do
                tyS <- typeOf s
                case tyS of
                    TypeSet{} -> na "rule_Freq"
                    _         -> return ()
            _ -> return ()
        return
            ( "Horizontal rule for mset-freq."
            , do
                 (iPat, i) <- quantifiedVar
                 return [essence| sum &iPat in &mset . toInt(&i = &arg) |]
            )


-- x in s ~~> or([ x = i | i in s ])
rule_In :: Rule
rule_In = "mset-in" `namedRule` theRule where
    theRule p = do
        (x,s)      <- match opIn p
        TypeMSet{} <- typeOf s
        return
            ( "Horizontal rule for mset-in."
            , do
                 (iPat, i) <- quantifiedVar
                 return [essence| exists &iPat in &s . &i = &x |]
            )


rule_Card :: Rule
rule_Card = "mset-card" `namedRule` theRule where
    theRule p = do
        s          <- match opTwoBars p
        TypeMSet{} <- typeOf s
        return
            ( "Horizontal rule for mset cardinality."
            , do
                (iPat, _) <- quantifiedVar
                return [essence| sum &iPat in &s . 1 |]
            )
