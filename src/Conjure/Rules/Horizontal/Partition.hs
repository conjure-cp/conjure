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


-- TODO: when _gofBefore and _gofAfter are /= []
rule_Comprehension_Literal :: Rule
rule_Comprehension_Literal = "partition-comprehension-literal" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (_gofBefore@[], (pat, expr), _gofAfter@[]) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Literal"
        let p = matchDef opParts expr
        elems <- match partitionLiteral p
        let f = lambdaToFunction pat body
        return
            ( "Comprehension on partition literals"
            , const $ AbstractLiteral $ AbsLitMatrix
                        (DomainInt [RangeBounded 1 (fromInt (length elems))])
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
        return ( "Horizontal rule for partition equality"
               , const $ make opAnd [ make opSubsetEq x y
                                    , make opSubsetEq y x
                                    ]
               )


rule_Neq :: Rule
rule_Neq = "partition-neq" `namedRule` theRule where
    theRule [essence| &x != &y |] = do
        TypePartition{} <- typeOf x
        TypePartition{} <- typeOf y
        return ( "Horizontal rule for partition dis-equality"
               , const [essence| !(&x = &y) |]
               )
    theRule _ = na "rule_Neq"


rule_SubsetEq :: Rule
rule_SubsetEq = "partition-subsetEq" `namedRule` theRule where
    theRule p = do
        (x,y)           <- match opSubsetEq p
        TypePartition{} <- typeOf x
        TypePartition{} <- typeOf y
        return ( "Horizontal rule for partition subsetEq"
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence| forAll &iPat in (&x) . &i in &y |]
               )


rule_Subset :: Rule
rule_Subset = "partition-subset" `namedRule` theRule where
    theRule [essence| &a subset &b |] = do
        TypePartition{} <- typeOf a
        TypePartition{} <- typeOf b
        return
            ( "Horizontal rule for partition subset"
            , const [essence| &a subsetEq &b /\ &a != &b |]
            )
    theRule _ = na "rule_Subset"


rule_Supset :: Rule
rule_Supset = "partition-supset" `namedRule` theRule where
    theRule [essence| &a supset &b |] = do
        TypePartition{} <- typeOf a
        TypePartition{} <- typeOf b
        return
            ( "Horizontal rule for partition supset"
            , const [essence| &b subset &a |]
            )
    theRule _ = na "rule_Supset"


rule_SupsetEq :: Rule
rule_SupsetEq = "partition-subsetEq" `namedRule` theRule where
    theRule [essence| &a supsetEq &b |] = do
        TypePartition{} <- typeOf a
        TypePartition{} <- typeOf b
        return
            ( "Horizontal rule for partition supsetEq"
            , const [essence| &b subsetEq &a |]
            )
    theRule _ = na "rule_SupsetEq"


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
        return ( "Horizontal rule for partition <" <+> pretty (make opLt ma mb)
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
        return ( "Horizontal rule for partition <=" <+> pretty (make opLeq ma mb)
               , const $ make opLeq ma mb
               )


rule_In :: Rule
rule_In = "partition-in" `namedRule` theRule where
    theRule p = do
        (x,s)           <- match opIn p
        TypePartition{} <- typeOf s
        return ( "Horizontal rule for partition-in."
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence| exists &iPat in parts(&s) . &i = &x |]
               )
