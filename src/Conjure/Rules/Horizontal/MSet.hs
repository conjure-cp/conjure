{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.MSet where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.TypeOf
import Conjure.Language.DomainOf
import Conjure.Language.Lenses
import Conjure.Language.TH

import Conjure.Rules.Definition ( Rule(..), namedRule, hasRepresentation )

import Conjure.Representations ( downX1 )


rule_Eq :: Rule
rule_Eq = "mset-eq" `namedRule` theRule where
    theRule p = do
        (x,y)      <- match opEq p
        TypeMSet{} <- typeOf x
        TypeMSet{} <- typeOf y
        return ( "Horizontal rule for mset equality"
               , const $ make opAnd [ make opSubsetEq x y
                                    , make opSubsetEq y x
                                    ]
               )


rule_Neq :: Rule
rule_Neq = "mset-neq" `namedRule` theRule where
    theRule [essence| &x != &y |] = do
        TypeMSet{} <- typeOf x
        TypeMSet{} <- typeOf y
        return ( "Horizontal rule for mset dis-equality"
               , const [essence| !(&x = &y) |]
               )
    theRule _ = na "rule_Neq"


rule_SubsetEq :: Rule
rule_SubsetEq = "mset-subsetEq" `namedRule` theRule where
    theRule p = do
        (x,y)      <- match opSubsetEq p
        DomainMSet _ _ inner <- domainOf x
        TypeMSet{}           <- typeOf y
        return ( "Horizontal rule for mset subsetEq"
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence| forAll &iPat : &inner . freq(&x,&i) <= freq(&y,&i) |]
               )


rule_Subset :: Rule
rule_Subset = "mset-subset" `namedRule` theRule where
    theRule [essence| &a subset &b |] = do
        TypeMSet{} <- typeOf a
        TypeMSet{} <- typeOf b
        return
            ( "Horizontal rule for mset subset"
            , const [essence| &a subsetEq &b /\ &a != &b |]
            )
    theRule _ = na "rule_Subset"


rule_Supset :: Rule
rule_Supset = "mset-supset" `namedRule` theRule where
    theRule [essence| &a supset &b |] = do
        TypeMSet{} <- typeOf a
        TypeMSet{} <- typeOf b
        return
            ( "Horizontal rule for mset supset"
            , const [essence| &b subset &a |]
            )
    theRule _ = na "rule_Supset"


rule_SupsetEq :: Rule
rule_SupsetEq = "mset-subsetEq" `namedRule` theRule where
    theRule [essence| &a supsetEq &b |] = do
        TypeMSet{} <- typeOf a
        TypeMSet{} <- typeOf b
        return
            ( "Horizontal rule for mset supsetEq"
            , const [essence| &b subsetEq &a |]
            )
    theRule _ = na "rule_SupsetEq"


rule_Lt :: Rule
rule_Lt = "mset-lt" `namedRule` theRule where
    theRule p = do
        (a,b)      <- match opLt p
        TypeMSet{} <- typeOf a
        TypeMSet{} <- typeOf b
        hasRepresentation a
        hasRepresentation b
        ma <- AbstractLiteral . AbsLitTuple <$> downX1 a
        mb <- AbstractLiteral . AbsLitTuple <$> downX1 b
        return ( "Horizontal rule for mset <" <+> pretty (make opLt ma mb)
               , const $ make opLt ma mb
               )


rule_Leq :: Rule
rule_Leq = "mset-leq" `namedRule` theRule where
    theRule p = do
        (a,b)      <- match opLeq p
        TypeMSet{} <- typeOf a
        TypeMSet{} <- typeOf b
        hasRepresentation a
        hasRepresentation b
        ma <- AbstractLiteral . AbsLitTuple <$> downX1 a
        mb <- AbstractLiteral . AbsLitTuple <$> downX1 b
        return ( "Horizontal rule for mset <=" <+> pretty (make opLeq ma mb)
               , const $ make opLeq ma mb
               )


rule_MaxMin :: Rule
rule_MaxMin = "mset-max-min" `namedRule` theRule where
    theRule [essence| max(&s) |] = do
        TypeMSet TypeInt <- typeOf s
        return
            ( "Horizontal rule for mset max"
            , \ fresh ->
                let (iPat, i) = quantifiedVar (fresh `at` 0)
                in  [essence| max([&i | &iPat <- &s]) |]
            )
    theRule [essence| min(&s) |] = do
        TypeMSet TypeInt <- typeOf s
        return
            ( "Horizontal rule for mset min"
            , \ fresh ->
                let (iPat, i) = quantifiedVar (fresh `at` 0)
                in  [essence| min([&i | &iPat <- &s]) |]
            )
    theRule _ = na "rule_MaxMin"


-- x in s ~~> or([ x = i | i in s ])
rule_In :: Rule
rule_In = "mset-in" `namedRule` theRule where
    theRule p = do
        (x,s)      <- match opIn p
        TypeMSet{} <- typeOf s
        return ( "Horizontal rule for mset-in."
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence| exists &iPat in &s . &i = &x |]
               )


rule_Card :: Rule
rule_Card = "mset-card" `namedRule` theRule where
    theRule p = do
        s          <- match opTwoBars p
        TypeMSet{} <- typeOf s
        return ( "Horizontal rule for mset cardinality."
               , \ fresh ->
                    let (iPat, _) = quantifiedVar (fresh `at` 0)
                    in  [essence| sum &iPat in &s . 1 |]
               )
