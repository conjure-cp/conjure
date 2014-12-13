{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.Relation where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Language.TypeOf
import Conjure.Language.Lenses
import Conjure.Language.TH

import Conjure.Rules.Definition ( Rule(..), namedRule )


rule_Eq :: Rule
rule_Eq = "relation-eq" `namedRule` theRule where
    theRule p = do
        (x,y)          <- match opEq p
        TypeRelation{} <- typeOf x
        TypeRelation{} <- typeOf y
        return ( "Horizontal rule for relation equality"
               , const $ make opEq (make opToSet x)
                                   (make opToSet y)
               )


rule_Neq :: Rule
rule_Neq = "relation-neq" `namedRule` theRule where
    theRule [essence| &x != &y |] = do
        TypeRelation{} <- typeOf x
        TypeRelation{} <- typeOf y
        return ( "Horizontal rule for relation dis-equality"
               , const [essence| !(&x = &y) |]
               )
    theRule _ = na "rule_Neq"


rule_SubsetEq :: Rule
rule_SubsetEq = "relation-subsetEq" `namedRule` theRule where
    theRule p = do
        (x,y)     <- match opSubsetEq p
        TypeRelation{} <- typeOf x
        TypeRelation{} <- typeOf y
        return ( "Horizontal rule for relation subsetEq"
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence| forAll &iPat in (&x) . &i in &y |]
               )


rule_Subset :: Rule
rule_Subset = "relation-subset" `namedRule` theRule where
    theRule [essence| &a subset &b |] = do
        TypeRelation{} <- typeOf a
        TypeRelation{} <- typeOf b
        return
            ( "Horizontal rule for relation subset"
            , const [essence| &a subsetEq &b /\ &a != &b |]
            )
    theRule _ = na "rule_Subset"


rule_Supset :: Rule
rule_Supset = "relation-supset" `namedRule` theRule where
    theRule [essence| &a supset &b |] = do
        TypeRelation{} <- typeOf a
        TypeRelation{} <- typeOf b
        return
            ( "Horizontal rule for relation supset"
            , const [essence| &b subset &a |]
            )
    theRule _ = na "rule_Supset"


rule_SupsetEq :: Rule
rule_SupsetEq = "relation-subsetEq" `namedRule` theRule where
    theRule [essence| &a supsetEq &b |] = do
        TypeRelation{} <- typeOf a
        TypeRelation{} <- typeOf b
        return
            ( "Horizontal rule for relation supsetEq"
            , const [essence| &b subsetEq &a |]
            )
    theRule _ = na "rule_SupsetEq"


rule_In :: Rule
rule_In = "relation-in" `namedRule` theRule where
    theRule [essence| &x in &rel |] = do
        TypeRelation{} <- typeOf rel
        return ( "relation membership to existential quantification"
               , \ fresh ->
                   let (iPat, i) = quantifiedVar (fresh `at` 0)
                   in  [essence| exists &iPat in toSet(&rel) . &i = &x |]
               )
    theRule _ = na "rule_In"
