{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.Set where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Language.Pretty
import Conjure.Language.TypeOf
import Conjure.Language.Lenses
import Conjure.Language.TH

import Conjure.Rules.Definition ( Rule(..), namedRule, hasRepresentation, matchFirst )

import Conjure.Representations ( downX1 )


rule_Set_Eq :: Rule
rule_Set_Eq = "set-eq" `namedRule` theRule where
    theRule p = do
        (x,y)     <- match opEq p
        TypeSet{} <- typeOf x
        TypeSet{} <- typeOf y
        return ( "Horizontal rule for set equality"
               , const $ make opAnd [ make opSubsetEq x y
                                    , make opSubsetEq y x
                                    ]
               )


rule_Set_Neq :: Rule
rule_Set_Neq = "set-neq" `namedRule` theRule where
    theRule [essence| &x != &y |] = do
        TypeSet{} <- typeOf x
        TypeSet{} <- typeOf y
        return ( "Horizontal rule for set dis-equality"
               , const [essence| !(&x = &y) |]
               )
    theRule _ = fail "No match."


rule_Set_SubsetEq :: Rule
rule_Set_SubsetEq = "set-subsetEq" `namedRule` theRule where
    theRule p = do
        (x,y)     <- match opSubsetEq p
        TypeSet{} <- typeOf x
        TypeSet{} <- typeOf y
        return ( "Horizontal rule for set subsetEq"
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence| forAll &iPat in (&x) . &i in &y |]
               )


rule_Set_Subset :: Rule
rule_Set_Subset = "set-subset" `namedRule` theRule where
    theRule [essence| &a subset &b |] =
        return
            ( "Horizontal rule for set subset"
            , const [essence| &a subsetEq &b /\ &a != &b |]
            )
    theRule _ = fail "No match."


rule_Set_Supset :: Rule
rule_Set_Supset = "set-supset" `namedRule` theRule where
    theRule [essence| &a supset &b |] =
        return
            ( "Horizontal rule for set supset"
            , const [essence| &b subset &a |]
            )
    theRule _ = fail "No match."


rule_Set_SupsetEq :: Rule
rule_Set_SupsetEq = "set-subsetEq" `namedRule` theRule where
    theRule [essence| &a supsetEq &b |] =
        return
            ( "Horizontal rule for set supsetEq"
            , const [essence| &b subsetEq &a |]
            )
    theRule _ = fail "No match."


rule_Set_Lt :: Rule
rule_Set_Lt = "set-lt" `namedRule` theRule where
    theRule p = do
        (a,b) <- match opLt p
        TypeSet{} <- typeOf a
        TypeSet{} <- typeOf b
        hasRepresentation a
        hasRepresentation b
        ma <- AbstractLiteral . AbsLitTuple <$> downX1 a
        mb <- AbstractLiteral . AbsLitTuple <$> downX1 b
        return ( "Horizontal rule for set <" <+> pretty (make opLt ma mb)
               , const $ make opLt ma mb
               )


rule_Set_Leq :: Rule
rule_Set_Leq = "set-leq" `namedRule` theRule where
    theRule p = do
        (a,b) <- match opLeq p
        TypeSet{} <- typeOf a
        TypeSet{} <- typeOf b
        hasRepresentation a
        hasRepresentation b
        ma <- AbstractLiteral . AbsLitTuple <$> downX1 a
        mb <- AbstractLiteral . AbsLitTuple <$> downX1 b
        return ( "Horizontal rule for set <=" <+> pretty (make opLeq ma mb)
               , const $ make opLeq ma mb
               )


rule_Set_Intersect :: Rule
rule_Set_Intersect = "set-intersect" `namedRule` theRule where
    theRule (Comprehension body gensOrFilters) = do
        (gofBefore, (pat, iPat, s), gofAfter) <- matchFirst gensOrFilters $ \ gof -> case gof of
            Generator (GenInExpr pat@(Single iPat) s) -> return (pat, iPat, s)
            _ -> fail "No match."
        (x, y)             <- match opIntersect s
        tx                 <- typeOf x
        case tx of
            TypeSet{}      -> return ()
            TypeMSet{}     -> return ()
            TypeFunction{} -> return ()
            TypeRelation{} -> return ()
            _              -> fail "type incompatibility in intersect operator"
        let i = Reference iPat Nothing
        return
            ( "Horizontal rule for set intersection"
            , const $
                Comprehension body
                    $  gofBefore
                    ++ [ Generator (GenInExpr pat x)
                       , Filter [essence| &i in &y |]
                       ]
                    ++ gofAfter
            )
    theRule _ = fail "No match."


rule_Set_Union :: Rule
rule_Set_Union = "set-union" `namedRule` theRule where
    theRule (Comprehension body gensOrFilters) = do
        (gofBefore, (pat, iPat, s), gofAfter) <- matchFirst gensOrFilters $ \ gof -> case gof of
            Generator (GenInExpr pat@(Single iPat) s) -> return (pat, iPat, s)
            _ -> fail "No match."
        (x, y)             <- match opUnion s
        tx                 <- typeOf x
        case tx of
            TypeSet{}      -> return ()
            TypeMSet{}     -> return ()
            TypeFunction{} -> return ()
            TypeRelation{} -> return ()
            _              -> fail "type incompatibility in union operator"
        let i = Reference iPat Nothing
        return
            ( "Horizontal rule for set union"
            , const $ make opFlatten $ AbstractLiteral $ AbsLitList
                [ Comprehension body
                    $  gofBefore
                    ++ [ Generator (GenInExpr pat x) ]
                    ++ gofAfter
                , Comprehension body
                    $  gofBefore
                    ++ [ Generator (GenInExpr pat y)
                       , Filter [essence| !(&i in &x) |]
                       ]
                    ++ gofAfter
                ]
            )
    theRule _ = fail "No match."


rule_Set_MaxMin :: Rule
rule_Set_MaxMin = "set-max-min" `namedRule` theRule where
    theRule [essence| max(&s) |] = do
        TypeSet TypeInt <- typeOf s
        return
            ( "Horizontal rule for set max"
            , \ fresh ->
                let (iPat, i) = quantifiedVar (fresh `at` 0)
                in  [essence| max([&i | &iPat <- &s]) |]
            )
    theRule [essence| min(&s) |] = do
        TypeSet TypeInt <- typeOf s
        return
            ( "Horizontal rule for set min"
            , \ fresh ->
                let (iPat, i) = quantifiedVar (fresh `at` 0)
                in  [essence| min([&i | &iPat <- &s]) |]
            )
    theRule _ = fail "No match."


-- x in s ~~> or([ x = i | i in s ])
-- where s is a set
-- and not Occurrence
rule_Set_In :: Rule
rule_Set_In = "set-in" `namedRule` theRule where
    theRule p = do
        (x,s)     <- match opIn p
        TypeSet{} <- typeOf s
        return ( "Horizontal rule for set-in."
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence| exists &iPat in &s . &i = &x |]
               )


rule_Set_Card :: Rule
rule_Set_Card = "set-card" `namedRule` theRule where
    theRule p = do
        s         <- match opTwoBars p
        TypeSet{} <- typeOf s
        return ( "Horizontal rule for set cardinality."
               , \ fresh ->
                    let (iPat, _) = quantifiedVar (fresh `at` 0)
                    in  [essence| sum &iPat in &s . 1 |]
               )
