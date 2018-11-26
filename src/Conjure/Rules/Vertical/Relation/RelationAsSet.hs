{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Relation.RelationAsSet where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "relation-map_in_expr{RelationAsSet}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, rel), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet, opToMSet] expr)
            _ -> na "rule_Comprehension"
        TypeRelation{}   <- typeOf rel
        Relation_AsSet{} <- representationOf rel
        [set]            <- downX1 rel
        return
            ( "Vertical rule for map_in_expr for relation domains, RelationAsSet representation."
            , return $
                Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat set) ]
                    ++ gocAfter
            )
    theRule _ = na "rule_Comprehension"

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
