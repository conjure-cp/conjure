{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Sequence.ExplicitBounded where

import Conjure.Rules.Import


rule_Card :: Rule
rule_Card = "sequence-cardinality{ExplicitBounded}" `namedRule` theRule where
    theRule [essence| |&s| |] = do
        TypeSequence{}    <- typeOf s
        "ExplicitBounded" <- representationOf s
        [sLength, _]      <- downX1 s
        return ( "Vertical rule for sequence cardinality."
               , const sLength
               )
    theRule _ = na "rule_Card"
