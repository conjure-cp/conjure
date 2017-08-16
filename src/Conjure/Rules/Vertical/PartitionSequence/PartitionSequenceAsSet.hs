module Conjure.Rules.Vertical.PartitionSequence.PartitionSequenceAsSet where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "partitionSequence-comprehension{PartitionSequenceAsSet}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet, opToMSet] expr)
            _ -> na "rule_Comprehension"
        let partitionSequence_    =  matchDef opParts expr
        TypePartitionSequence{}   <- typeOf partitionSequence_
        PartitionSequence_AsSet{} <- representationOf partitionSequence_
        [s]               <- downX1 partitionSequence_
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for partitionSequence-comprehension, PartitionSequenceAsSet representation"
            , do
                (jPat, j) <- quantifiedVar
                let val = j
                return $ Comprehension (upd val body)
                        $  gocBefore
                        ++ [ Generator (GenInExpr jPat s) ]
                        ++ transformBi (upd val) gocAfter
            )
    theRule _ = na "rule_Comprehension"
