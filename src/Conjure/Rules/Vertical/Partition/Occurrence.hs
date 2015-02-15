{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Partition.Occurrence where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.DomainOf
import Conjure.Language.Type
import Conjure.Language.TypeOf
import Conjure.Language.Lenses
import Conjure.Language.TH

import Conjure.Rules.Definition ( Rule(..), namedRule, representationOf, matchFirst )

import Conjure.Representations ( downX1 )


rule_Comprehension :: Rule
rule_Comprehension = "partition-comprehension{Occurrence}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension"
        partition               <- match opParts expr
        TypePartition{}         <- typeOf partition
        "Occurrence"            <- representationOf partition
        [flags, parts, nbParts] <- downX1 partition
        indexDom                <- forgetRepr <$> domainOf nbParts
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for partition-comprehension, Occurrence representation"
            , \ fresh ->
                let (pPat, p) = quantifiedVar (fresh `at` 0)
                    (iPat, i) = quantifiedVar (fresh `at` 1)
                    -- the value, a set representing the i'th part
                    val = make opToSet $ Comprehension i
                        [ Generator (GenDomainNoRepr iPat indexDom)
                        , Condition [essence| &flags[&i] |]
                        , Condition [essence| &parts[&i] = &p |]
                        ]
                in
                    Comprehension (upd val body)
                        $  gofBefore
                        ++ [ Generator (GenDomainNoRepr pPat indexDom)          -- part number p
                           , Condition [essence| &p <= &nbParts |]
                           ]
                        ++ transformBi (upd val) gofAfter
            )
    theRule _ = na "rule_Comprehension"
