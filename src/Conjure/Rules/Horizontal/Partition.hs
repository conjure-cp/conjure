{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.Partition where

import Conjure.Rules.Import


rule_Comprehension_Literal :: Rule
rule_Comprehension_Literal = "partition-comprehension-literal" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, p), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDef opParts expr)
            _ -> na "rule_Comprehension_Literal"
        (TypePartition tau, elems) <- match partitionLiteral p
        let outLiteral = make matrixLiteral
                            (TypeMatrix (TypeInt TagInt) (TypeSet tau))
                            (DomainInt TagInt [RangeBounded 1 (fromInt (genericLength elems))])
                            [ AbstractLiteral (AbsLitSet e)
                            | e <- elems
                            ]
        let upd val old = lambdaToFunction pat old val
        return
            ( "Comprehension on partition literals"
            , do
                 (iPat, i) <- quantifiedVar
                 return $ Comprehension (upd i body)
                         $  gocBefore
                         ++ [Generator (GenInExpr iPat outLiteral)]
                         ++ transformBi (upd i) gocAfter
            )
    theRule _ = na "rule_Comprehension_PartitionLiteral"


rule_Eq :: Rule
rule_Eq = "partition-eq" `namedRule` theRule where
    theRule [essence| &x = &y |] = do
        TypePartition{} <- typeOf x
        TypePartition{} <- typeOf y
        return
            ( "Horizontal rule for partition equality"
            , return [essence| |&x| = |&y| /\ parts(&x) = parts(&y) |]
            )
    theRule _ = na "rule_Eq"


rule_Neq :: Rule
rule_Neq = "partition-neq" `namedRule` theRule where
    theRule [essence| &x != &y |] = do
        TypePartition{} <- typeOf x
        TypePartition{} <- typeOf y
        return
            ( "Horizontal rule for partition dis-equality"
               , do
                    (iPat, i) <- quantifiedVar
                    return [essence|
                            (exists &iPat in &x . !(&i in &y))
                            \/
                            (exists &iPat in &y . !(&i in &x))
                        |]
            )
    theRule _ = na "rule_Neq"


rule_Together :: Rule
rule_Together = "partition-together" `namedRule` theRule where
    theRule [essence| together(&x,&p) |] = do
        TypePartition{} <- typeOf p
        DomainPartition _ _ inner <- domainOf p
        return
            ( "Horizontal rule for partition-together"
            , do
                 (iPat, i) <- quantifiedVar
                 (jPat, j) <- quantifiedVar
                 (kPat, k) <- quantifiedVar
                 return [essence|
                             (exists &iPat in parts(&p) . &x subsetEq &i)
                             /\
                             $ the items in x appear somewhere in the partition
                             (forAll &jPat in &x . exists &kPat : &inner . &j = &k)
                        |]
            )
    theRule _ = na "rule_Together"


rule_Apart :: Rule
rule_Apart = "partition-apart" `namedRule` theRule where
    theRule [essence| apart(&x,&p) |] = do
        case p of
            -- this is because this rule would change the parity of the DefinednessConstraints
            -- they should be bubbled up first.
            WithLocals{} -> na "rule_Apart"
            _ -> return ()
        TypePartition{} <- typeOf p
        DomainPartition _ _ inner <- domainOf p
        return
            ( "Horizontal rule for partition-apart"
            , do
                (iPat, i) <- quantifiedVar
                (jPat, j) <- quantifiedVar
                (kPat, k) <- quantifiedVar
                return [essence|
                             (forAll &iPat in parts(&p) . !(&x subsetEq &i))
                                    /\
                             $ the items in x appear somewhere in the partition
                             (forAll &jPat in &x . exists &kPat : &inner . &j = &k)
                       |]
            )
    theRule _ = na "rule_Apart"


rule_Party :: Rule
rule_Party = "partition-party" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Literal"
        (mkModifier, expr2) <- match opModifier expr
        (wanted, p)         <- match opParty expr2
        TypePartition{} <- typeOf p
        let upd val old = lambdaToFunction pat old val
        return
            ( "Comprehension on a particular part of a partition"
            , do
                 (iPat, i) <- quantifiedVar
                 (jPat, j) <- quantifiedVar
                 return $
                     Comprehension (upd j body)
                         $  gocBefore
                         ++ [ Generator (GenInExpr iPat (make opParts p))
                            , Condition [essence| &wanted in &i |]
                            , Generator (GenInExpr jPat (mkModifier i))
                            ]
                         ++ transformBi (upd j) gocAfter
            )
    theRule _ = na "rule_Party"


rule_Participants :: Rule
rule_Participants = "partition-participants" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Literal"
        p <- match opParticipants expr
        TypePartition{} <- typeOf p
        let upd val old = lambdaToFunction pat old val
        return
            ( "Comprehension on participants of a partition"
            , do
                 (iPat, i) <- quantifiedVar
                 (jPat, j) <- quantifiedVar
                 return $ Comprehension (upd j body)
                         $  gocBefore
                         ++ [ Generator (GenInExpr iPat (make opParts p))
                            , Generator (GenInExpr jPat i)
                            ]
                         ++ transformBi (upd j) gocAfter
            )
    theRule _ = na "rule_Participants"


rule_Card :: Rule
rule_Card = "partition-card" `namedRule` theRule where
    theRule p = do
        partition_      <- match opTwoBars p
        TypePartition{} <- typeOf partition_
        return
            ( "Partition cardinality"
            , do
                dom <- runMaybeT $ domainOf partition_
                case dom of
                    Just (DomainPartition _ (PartitionAttr (SizeAttr_Size nbParts) (SizeAttr_Size partSize) _) _)
                        -> return [essence| &nbParts * &partSize |]
                    _ -> return [essence| |participants(&partition_)| |]
            )


rule_In :: Rule
rule_In = "partition-in" `namedRule` theRule where
    theRule [essence| &x in &p |] = do
        TypePartition{} <- typeOf p
        return
            ( "Horizontal rule for partition-in."
            , return [essence| &x in parts(&p) |]
            )
    theRule _ = na "rule_In"
