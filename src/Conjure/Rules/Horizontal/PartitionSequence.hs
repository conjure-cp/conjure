{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.PartitionSequence where

import Conjure.Rules.Import


rule_Comprehension_Literal :: Rule
rule_Comprehension_Literal = "partitionSequence-comprehension-literal" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, p), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDef opParts expr)
            _ -> na "rule_Comprehension_Literal"
        (TypePartitionSequence tau, elems) <- match partitionSequenceLiteral p
        let outLiteral = make matrixLiteral
                            (TypeMatrix TypeInt (TypeSet tau))
                            (DomainInt [RangeBounded 1 (fromInt (genericLength elems))])
                            [ AbstractLiteral (AbsLitSet e)
                            | e <- elems
                            ]
        let upd val old = lambdaToFunction pat old val
        return
            ( "Comprehension on partitionSequence literals"
            , do
                 (iPat, i) <- quantifiedVar
                 return $ Comprehension (upd i body)
                         $  gocBefore
                         ++ [Generator (GenInExpr iPat outLiteral)]
                         ++ transformBi (upd i) gocAfter
            )
    theRule _ = na "rule_Comprehension_PartitionSequenceLiteral"


rule_Eq :: Rule
rule_Eq = "partitionSequence-eq" `namedRule` theRule where
    theRule p = do
        (x,y)           <- match opEq p
        TypePartitionSequence{} <- typeOf x
        TypePartitionSequence{} <- typeOf y
        return
            ( "Horizontal rule for partitionSequence equality"
            , return $ make opEq (make opParts x) (make opParts y)
            )


rule_Neq :: Rule
rule_Neq = "partitionSequence-neq" `namedRule` theRule where
    theRule [essence| &x != &y |] = do
        TypePartitionSequence{} <- typeOf x
        TypePartitionSequence{} <- typeOf y
        return
            ( "Horizontal rule for partitionSequence dis-equality"
               , do
                    (iPat, i) <- quantifiedVar
                    return [essence|
                            (exists &iPat in &x . !(&i in &y))
                            \/
                            (exists &iPat in &y . !(&i in &x))
                        |]
            )
    theRule _ = na "rule_Neq"


rule_DotLt :: Rule
rule_DotLt = "partitionSequence-DotLt" `namedRule` theRule where
    theRule p = do
        (a,b)           <- match opDotLt p
        TypePartitionSequence{} <- typeOf a
        TypePartitionSequence{} <- typeOf b
        sameRepresentation a b
        ma <- tupleLitIfNeeded <$> downX1 a
        mb <- tupleLitIfNeeded <$> downX1 b
        return
            ( "Horizontal rule for partitionSequence .<" <+> pretty (make opDotLt ma mb)
            , return $ make opDotLt ma mb
            )


rule_DotLeq :: Rule
rule_DotLeq = "partitionSequence-DotLeq" `namedRule` theRule where
    theRule p = do
        (a,b)           <- match opDotLeq p
        TypePartitionSequence{} <- typeOf a
        TypePartitionSequence{} <- typeOf b
        sameRepresentation a b
        ma <- tupleLitIfNeeded <$> downX1 a
        mb <- tupleLitIfNeeded <$> downX1 b
        return
            ( "Horizontal rule for partitionSequence .<=" <+> pretty (make opDotLeq ma mb)
            , return $ make opDotLeq ma mb
            )


rule_Together :: Rule
rule_Together = "partitionSequence-together" `namedRule` theRule where
    theRule [essence| together(&x,&p) |] = do
        TypePartitionSequence{} <- typeOf p
        return
            ( "Horizontal rule for partitionSequence-together"
            , do
                 (xiPat, xi) <- quantifiedVar
                 (iPat , i ) <- quantifiedVar
                 (jPat , j ) <- quantifiedVar
                 return [essence| exists &iPat in parts(&p) .
                                     forAll &xiPat in &x .
                                         exists &jPat in &i . &xi = &j[2]
                                |]
            )
    theRule _ = na "rule_Together"


rule_Apart :: Rule
rule_Apart = "partitionSequence-apart" `namedRule` theRule where
    theRule [essence| apart(&x,&p) |] = do
        case p of
            -- this is because this rule would change the parity of the DefinednessConstraints
            -- they should be bubbled up first.
            WithLocals{} -> na "rule_Apart"
            _ -> return ()
        TypePartitionSequence{} <- typeOf p
        return
            ( "Horizontal rule for partitionSequence-apart"
            , return [essence| !together(&x,&p) |]
            )
    theRule _ = na "rule_Apart"


rule_Party :: Rule
rule_Party = "partitionSequence-party" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Literal"
        (mkModifier, expr2) <- match opModifier expr
        (wanted, p)         <- match opParty expr2
        let upd val old = lambdaToFunction pat old val
        return
            ( "Comprehension on a particular part of a partitionSequence"
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
rule_Participants = "partitionSequence-participants" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Literal"
        p <- match opParticipants expr
        let upd val old = lambdaToFunction pat old val
        return
            ( "Comprehension on participants of a partitionSequence"
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
rule_Card = "partitionSequence-card" `namedRule` theRule where
    theRule p = do
        partitionSequence_      <- match opTwoBars p
        TypePartitionSequence{} <- typeOf partitionSequence_
        return
            ( "Cardinality of a partitionSequence"
            , return $ make opTwoBars $ make opParticipants partitionSequence_
            )


rule_In :: Rule
rule_In = "partitionSequence-in" `namedRule` theRule where
    theRule [essence| &x in &p |] = do
        TypePartitionSequence{} <- typeOf p
        return
            ( "Horizontal rule for partitionSequence-in."
            , return [essence| &x in parts(&p) |]
            )
    theRule _ = na "rule_In"
