module Conjure.Rules.Vertical.Partition.PartitionAsSet where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Language.TypeOf
import Conjure.Language.Lenses

import Conjure.Rules.Definition ( Rule(..), namedRule, representationOf, matchFirst )

import Conjure.Representations ( downX1 )


rule_Comprehension :: Rule
rule_Comprehension = "partition-comprehension{PartitionAsSet}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension"
        p                    <- match opParts expr
        TypePartition{}      <- typeOf p
        "PartitionAsSet"     <- representationOf p
        [s]                  <- downX1 p
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for partition-comprehension, PartitionAsSet representation"
            , \ fresh ->
                let (jPat, j) = quantifiedVar (fresh `at` 0)
                    val = j
                in
                    Comprehension (upd val body)
                        $  gofBefore
                        ++ [ Generator (GenInExpr jPat s) ]
                        ++ transformBi (upd val) gofAfter
            )
    theRule _ = na "rule_Comprehension"
