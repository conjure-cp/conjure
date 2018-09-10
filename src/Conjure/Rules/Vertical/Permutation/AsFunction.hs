{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Permutation.AsFunction where

import Conjure.Rules.Import

-- | This just unwraps the function in a comprehension
-- Just like having the bare bijective total function in the comprehension
--
-- unclear if this should stay but it is here for now
rule_Comprehension :: Rule
rule_Comprehension = "permutation-comprehension{AsFunction}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, perm), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet, opToMSet] expr)
            _ -> na "rule_Comprehension"
        TypePermutation{}                 <- typeOf perm
        Permutation_AsFunction <- representationOf perm
        [func] <- downX1 perm 
        return
            ( "Vertical rule for permutation-comprehension, AsFunction representation"
            , do
                return $ Comprehension body 
                        $  gocBefore
                        ++ [ Generator (GenInExpr pat func)
                           ]
                        ++ gocAfter 
            )
    theRule _ = na "rule_Comprehension"


-- | Want this to be
-- p(i) becomes [i, f(i)][toInt(i in defined(f))]
rule_Application :: Rule
rule_Application = error "permutation: rule_Application not defined yet."
