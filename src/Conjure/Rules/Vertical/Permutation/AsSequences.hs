{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Permutation.AsSequences where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "permutation-comprehension{AsSequences}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, perm), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet, opToMSet] expr)
            _ -> na "rule_Comprehension"
        TypePermutation{}                 <- typeOf perm
        Permutation_AsSequences <- representationOf perm
        [setOfSequences] <- downX1 perm 
        return
            ( "Vertical rule for permutation-comprehension, AsSequences representation"
            , do
                return $ Comprehension body 
                        $  gocBefore
                        ++ [ Generator (GenInExpr pat setOfSequences)
                           ]
                        ++ gocAfter 
            )
    theRule _ = na "rule_Comprehension"
