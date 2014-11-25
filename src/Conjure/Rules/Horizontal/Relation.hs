{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.Relation where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Language.TypeOf
import Conjure.Language.Lenses
import Conjure.Language.TH

import Conjure.Rules.Definition ( Rule(..), namedRule )


rule_Relation_Eq :: Rule
rule_Relation_Eq = "relation-eq" `namedRule` theRule where
    theRule p = do
        (x,y)          <- match opEq p
        TypeRelation{} <- typeOf x
        TypeRelation{} <- typeOf y
        return ( "Horizontal rule for relation equality"
               , const $ make opEq (make opToSet x)
                                   (make opToSet y)
               )


rule_Relation_In :: Rule
rule_Relation_In = "relation-in" `namedRule` theRule where
    theRule [essence| &x in &rel |] = do
        TypeRelation{} <- typeOf rel
        return ( "relation membership to existential quantification"
               , \ fresh ->
                   let (iPat, i) = quantifiedVar (fresh `at` 0)
                   in  [essence| exists &iPat in toSet(&rel) . &i = &x |]
               )
    theRule _ = fail "No match."
