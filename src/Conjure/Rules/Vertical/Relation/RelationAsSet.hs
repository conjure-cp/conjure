{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Relation.RelationAsSet where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "relation-comprehension{RelationAsSet}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, rel), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet, opToMSet] expr)
            _ -> na "rule_Comprehension"
        TypeRelation{}   <- typeOf rel
        Relation_AsSet{} <- representationOf rel
        [set]            <- downX1 rel
        return
            ( "Vertical rule for comprehension for relation domains, RelationAsSet representation."
            , return $
                Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat set) ]
                    ++ gocAfter
            )
    theRule _ = na "rule_Comprehension"


rule_PowerSet_Comprehension :: Rule
rule_PowerSet_Comprehension = "relation-powerSet-comprehension{RelationAsSet}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat expr) -> return (pat, expr)
            _ -> na "rule_Comprehension"
        rel <- matchDefs [opToSet,opToMSet,opToRelation] <$> match opPowerSet expr
        Relation_AsSet{} <- representationOf rel
        [set] <- downX1 rel
        return
            ( "Vertical rule for powerSet comprehension for relation domains, RelationAsSet representation."
            , return $
                Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat (make opPowerSet set)) ]
                    ++ gocAfter
            )
    theRule _ = na "rule_PowerSet_Comprehension"


rule_Card :: Rule
rule_Card = "relation-card{RelationAsSet}" `namedRule` theRule where
    theRule p = do
        rel              <- match opTwoBars p
        TypeRelation{}   <- typeOf rel
        Relation_AsSet{} <- representationOf rel
        [set]            <- downX1 rel
        return
            ( "Vertical rule for set cardinality, ExplicitVarSizeWithMarker representation."
            , return [essence| |&set| |]
            )
