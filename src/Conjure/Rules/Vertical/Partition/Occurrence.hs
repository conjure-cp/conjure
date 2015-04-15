{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Partition.Occurrence where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "partition-comprehension{Occurrence}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet, opToMSet] expr)
            _ -> na "rule_Comprehension"
        partition_              <- match opParts expr
        TypePartition{}         <- typeOf partition_
        "Occurrence"            <- representationOf partition_
        [flags, parts, nbParts] <- downX1 partition_
        indexDom                <- forgetRepr <$> domainOf nbParts
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for partition-comprehension, Occurrence representation"
            , do
                (pPat, p) <- quantifiedVar
                (iPat, i) <- quantifiedVar
                    -- the value, a set representing the i'th part
                let val = make opToSet $ Comprehension i
                        [ Generator (GenDomainNoRepr iPat indexDom)
                        , Condition [essence| &flags[&i] |]
                        , Condition [essence| &parts[&i] = &p |]
                        ]
                return $ Comprehension (upd val body)
                        $  gocBefore
                        ++ [ Generator (GenDomainNoRepr pPat indexDom)          -- part number p
                           , Condition [essence| &p <= &nbParts |]
                           ]
                        ++ transformBi (upd val) gocAfter
            )
    theRule _ = na "rule_Comprehension"
