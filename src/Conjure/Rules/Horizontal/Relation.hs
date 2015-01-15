{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.Relation where

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
rule_Comprehension_Literal = "relation-comprehension-literal" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (_gofBefore@[], (pat, expr), _gofAfter@[]) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Literal"
        elems <- match relationLiteral expr
        let f = lambdaToFunction pat body
        return
            ( "Comprehension on relation literals"
            , const $ AbstractLiteral $ AbsLitMatrix
                        (DomainInt [RangeBounded 1 (fromInt (length elems))])
                        [ f lit
                        | e <- elems
                        , let lit = AbstractLiteral (AbsLitTuple e)
                        ]
            )
    theRule _ = na "rule_Comprehension_Literal"


-- [ body | i <- rel(...) ]
-- [ body | jPat <- rel(...), j =   ]
rule_Comprehension_Projection :: Rule
rule_Comprehension_Projection = "relation-comprehension-projection" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Projection"
        (rel, args) <- match opRelationProj expr
        let upd val old = lambdaToFunction pat old val
        return
            ( "Comprehension on relation literals"
            , \ fresh ->
                let
                    (jPat, j) = quantifiedVar (fresh `at` 0)
                    -- those indices to keep
                    val = AbstractLiteral $ AbsLitTuple
                        [ [essence| &j[&iExpr] |]
                        | (i, Nothing) <- zip allNats args
                        , let iExpr = fromInt i
                        ]
                    conditions =
                        [ Condition [essence| &j[&iExpr] = &arg |]
                        | (i, Just arg) <- zip allNats args
                        , let iExpr = fromInt i
                        ]
                in
                    Comprehension
                       (upd val body)
                       $  gofBefore
                       ++ [Generator (GenInExpr jPat rel)]
                       ++ conditions
                       ++ transformBi (upd val) gofAfter
               )
    theRule _ = na "rule_Comprehension_Projection"


rule_PowerSet_Comprehension :: Rule
rule_PowerSet_Comprehension = "relation-powerSet-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (setPat, setPatNum, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr setPat@(AbsPatSet pats) expr) -> return (setPat, length pats, expr)
            _ -> na "rule_PowerSet_Comprehension"
        let upd val old      = lambdaToFunction setPat old val
        rel                  <- match opPowerSet expr
        TypeRelation{}       <- typeOf rel
        return
            ( "Horizontal rule for powerSet relation-comprehension"
            , \ fresh ->
                let outPats =
                        [ quantifiedVar (fresh `at` i) | i <- take setPatNum allNats ]
                    val = AbstractLiteral $ AbsLitSet
                        [ j | (_,j) <- outPats ]
                in
                    Comprehension (upd val body) $ concat
                        [ gofBefore
                        , concat
                            [ [ Generator (GenInExpr pat rel) ]
                            | (pat,_) <- take 1 outPats
                            ]
                        , concat
                            [ [ Generator (GenInExpr pat rel)
                              , Condition [essence| &beforeX < &patX |]
                              ]
                            | ((_, beforeX), (pat, patX)) <- zip outPats (tail outPats)
                            ]
                        , transformBi (upd val) gofAfter
                        ]
            )
    theRule _ = na "rule_PowerSet_Comprehension"


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


rule_Lt :: Rule
rule_Lt = "relation-lt" `namedRule` theRule where
    theRule p = do
        (a,b)          <- match opLt p
        TypeRelation{} <- typeOf a
        TypeRelation{} <- typeOf b
        hasRepresentation a
        hasRepresentation b
        ma <- tupleLitIfNeeded <$> downX1 a
        mb <- tupleLitIfNeeded <$> downX1 b
        return ( "Horizontal rule for relation <" <+> pretty (make opLt ma mb)
               , const $ make opLt ma mb
               )


rule_Leq :: Rule
rule_Leq = "relation-leq" `namedRule` theRule where
    theRule p = do
        (a,b)          <- match opLeq p
        TypeRelation{} <- typeOf a
        TypeRelation{} <- typeOf b
        hasRepresentation a
        hasRepresentation b
        ma <- tupleLitIfNeeded <$> downX1 a
        mb <- tupleLitIfNeeded <$> downX1 b
        return ( "Horizontal rule for relation <=" <+> pretty (make opLeq ma mb)
               , const $ make opLeq ma mb
               )


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
