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
                            (TypeMatrix (TypeInt NoTag) tau)
                            (DomainInt NoTag [RangeBounded 1 (fromInt (genericLength elems))])
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


rule_Comprehension_ToSet_Literal :: Rule
rule_Comprehension_ToSet_Literal = "mset-comprehension-toSet-literal" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_ToSet_Literal"
        mset                  <- match opToSet expr
        (TypeMSet tau, elems) <- match msetLiteral mset
        let outLiteralDomain = mkDomainIntB 1 (fromInt $ genericLength elems)
        let outLiteral = make matrixLiteral (TypeMatrix (TypeInt NoTag) tau) outLiteralDomain elems
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
            , do
                (iPat, i) <- quantifiedVar
                return [essence| max([&i | &iPat <- &s]) |]
            )
    theRule [essence| min(&s) |] = do
        TypeMSet (TypeInt _) <- typeOf s
        return
            ( "Horizontal rule for mset min"
            , do
                (iPat, i) <- quantifiedVar
                return [essence| min([&i | &iPat <- &s]) |]
            )
    theRule _ = na "rule_MaxMin"


-- freq(mset,arg) ~~> sum([ toInt(arg = i) | i in mset ])
rule_Freq :: Rule
rule_Freq = "mset-freq" `namedRule` theRule where
    theRule p = do
        (mset, arg) <- match opFreq p
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
