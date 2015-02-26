{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.Partition where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.Pretty
import Conjure.Language.TypeOf
import Conjure.Language.Lenses
import Conjure.Language.TH

import Conjure.Rules.Definition ( Rule(..), namedRule, hasRepresentation, matchFirst )

import Conjure.Representations ( downX1 )


-- TODO: when _gocBefore and _gocAfter are /= []
rule_Comprehension_Literal :: Rule
rule_Comprehension_Literal = "partition-comprehension-literal" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (_gocBefore@[], (pat, p), _gocAfter@[]) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDef opParts expr)
            _ -> na "rule_Comprehension_Literal"
        (TypePartition _, elems) <- match partitionLiteral p
        let f = lambdaToFunction pat body
        return
            ( "Comprehension on partition literals"
            , const $ make matrixLiteral
                        (TypeMatrix TypeInt TypeBool)
                        (DomainInt [RangeBounded 1 (fromInt (genericLength elems))])
                        [ f lit
                        | e <- elems
                        , let lit = AbstractLiteral (AbsLitSet e)
                        ]
            )
    theRule _ = na "rule_Comprehension_PartitionLiteral"


rule_Eq :: Rule
rule_Eq = "partition-eq" `namedRule` theRule where
    theRule p = do
        (x,y)           <- match opEq p
        TypePartition{} <- typeOf x
        TypePartition{} <- typeOf y
        return
            ( "Horizontal rule for partition equality"
            , const $ make opEq (make opParts x) (make opParts y)
            )


rule_Neq :: Rule
rule_Neq = "partition-neq" `namedRule` theRule where
    theRule [essence| &x != &y |] = do
        TypePartition{} <- typeOf x
        TypePartition{} <- typeOf y
        return
            ( "Horizontal rule for partition dis-equality"
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence|
                            (exists &iPat in &x . !(&i in &y))
                            \/
                            (exists &iPat in &y . !(&i in &x))
                        |]
            )
    theRule _ = na "rule_Neq"


rule_Lt :: Rule
rule_Lt = "partition-lt" `namedRule` theRule where
    theRule p = do
        (a,b)           <- match opLt p
        TypePartition{} <- typeOf a
        TypePartition{} <- typeOf b
        hasRepresentation a
        hasRepresentation b
        ma <- tupleLitIfNeeded <$> downX1 a
        mb <- tupleLitIfNeeded <$> downX1 b
        return
            ( "Horizontal rule for partition <" <+> pretty (make opLt ma mb)
            , const $ make opLt ma mb
            )


rule_Leq :: Rule
rule_Leq = "partition-leq" `namedRule` theRule where
    theRule p = do
        (a,b)           <- match opLeq p
        TypePartition{} <- typeOf a
        TypePartition{} <- typeOf b
        hasRepresentation a
        hasRepresentation b
        ma <- tupleLitIfNeeded <$> downX1 a
        mb <- tupleLitIfNeeded <$> downX1 b
        return
            ( "Horizontal rule for partition <=" <+> pretty (make opLeq ma mb)
            , const $ make opLeq ma mb
            )


rule_Together :: Rule
rule_Together = "partition-together" `namedRule` theRule where
    theRule [essence| together(&x,&y,&p) |] = do
        TypePartition{} <- typeOf p
        return
            ( "Horizontal rule for partition-together"
            , \ fresh ->
                 let (iPat, i) = quantifiedVar (fresh `at` 0)
                 in  [essence| exists &iPat in parts(&p) . &x in &i /\ &y in &i |]
            )
    theRule _ = na "rule_Together"


rule_Apart :: Rule
rule_Apart = "partition-apart" `namedRule` theRule where
    theRule [essence| apart(&x,&y,&p) |] = do
        TypePartition{} <- typeOf p
        return
            ( "Horizontal rule for partition-apart"
            , const [essence| !together(&x,&y,&p) /\ &x in participants(&p) /\ &y in participants(&p) |]
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
        let upd val old = lambdaToFunction pat old val
        return
            ( "Comprehension on participants of a partition"
            , \ fresh ->
                 let (iPat, i) = quantifiedVar (fresh `at` 0)
                     (jPat, j) = quantifiedVar (fresh `at` 1)
                 in  Comprehension (upd j body)
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
        let upd val old = lambdaToFunction pat old val
        return
            ( "Comprehension on participants of a partition"
            , \ fresh ->
                 let (iPat, i) = quantifiedVar (fresh `at` 0)
                     (jPat, j) = quantifiedVar (fresh `at` 1)
                 in  Comprehension (upd j body)
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
        partition       <- match opTwoBars p
        TypePartition{} <- typeOf partition
        return
            ( "Cardinality of a partition"
            , const $ make opTwoBars $ make opParticipants partition
            )


rule_In :: Rule
rule_In = "partition-in" `namedRule` theRule where
    theRule [essence| &x in &p |] = do
        TypePartition{} <- typeOf p
        return ( "Horizontal rule for partition-in."
               , const [essence| &x in parts(&p) |]
               )
    theRule _ = na "rule_In"
