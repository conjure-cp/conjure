{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Partition.Occurrence where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "partition-comprehension{Occurrence}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet, opToMSet] expr)
            _ -> na "rule_Comprehension"
        let partition_                  =  matchDef opParts expr
        TypePartition{}                 <- typeOf partition_
        DomainPartition _ _ innerDomain <- domainOf partition_
        Partition_Occurrence            <- representationOf partition_
        [numPartsVar, whichPart, _partSizesVar, _firstIndexVar] <- downX1 partition_
        indexDom                        <- forgetRepr <$> domainOf numPartsVar
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for partition-comprehension, Occurrence representation"
            , do
                (pPat, p) <- quantifiedVar
                (kPat, k) <- quantifiedVar
                -- the value, a set representing part number p
                -- indicate that there won't be duplicates in this comprehension!
                let val = make opToSetWithFlag True $ Comprehension k
                        [ Generator (GenDomainNoRepr kPat innerDomain)
                        , Condition [essence| &whichPart[&k] = &p |]
                        ]
                return $ Comprehension (upd val body)
                        $  gocBefore
                        ++ [ Generator (GenDomainNoRepr pPat indexDom)          -- part number p
                           , Condition [essence| &p <= &numPartsVar |]
                           ]
                        ++ transformBi (upd val) gocAfter
            )
    theRule _ = na "rule_Comprehension"
