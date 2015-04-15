module Conjure.Rules.Vertical.Partition.PartitionAsSet where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "partition-comprehension{PartitionAsSet}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet, opToMSet] expr)
            _ -> na "rule_Comprehension"
        let p                =  matchDef opParts expr
        TypePartition{}      <- typeOf p
        "PartitionAsSet"     <- representationOf p
        [s]                  <- downX1 p
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for partition-comprehension, PartitionAsSet representation"
            , do
                (jPat, j) <- quantifiedVar
                let val = j
                return $ Comprehension (upd val body)
                        $  gocBefore
                        ++ [ Generator (GenInExpr jPat s) ]
                        ++ transformBi (upd val) gocAfter
            )
    theRule _ = na "rule_Comprehension"
