module Conjure.Rules.Vertical.Relation.RelationAsSet where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Language.TypeOf

import Conjure.Rules.Definition ( Rule(..), namedRule, representationOf, matchFirst )

import Conjure.Representations ( downX1 )


rule_Comprehension :: Rule
rule_Comprehension = "relation-map_in_expr{RelationAsSet}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, rel), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension"
        TypeRelation{}         <- typeOf rel
        "RelationAsSet"        <- representationOf rel
        [set]                  <- downX1 rel

        -- let out fresh = unroll m [] (zip [ quantifiedVar fr TypeInt | fr <- fresh ] mIndices)
        return
            ( "Vertical rule for map_in_expr for relation domains, RelationAsSet representation."
            , const $
                Comprehension body
                    $  gofBefore
                    ++ [ Generator (GenInExpr pat set) ]
                    ++ gofAfter
            )
    theRule _ = na "rule_Comprehension"
