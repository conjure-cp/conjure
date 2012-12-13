{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Evaluator.DataAboutQuantifiers where

import Language.E.Imports
import Language.E.Definition
import Language.E.Helpers
import Language.E.CompE
import Language.E.TH
import Language.E.Pretty



identityOp :: MonadConjure m => Text -> m E
identityOp quantifier = case quantifier of
                "forAll" -> return [eMake| true  |]
                "exists" -> return [eMake| false |]
                "sum"    -> return [eMake| 0     |]
                _        -> err ErrFatal $ "Unknown quantifier: " <+> pretty quantifier


guardOp :: MonadConjure m => Text -> [E] -> E -> m E
guardOp _ [] b = return b
guardOp _ [ [xMatch| [] := emptyGuard |] ] b = return b
guardOp quantifier as b =
    let a = conjunct as
    in  case quantifier of
            "forAll" -> return [eMake|       &a  -> &b |]
            "exists" -> return [eMake|       &a  /\ &b |]
            "sum"    -> return [eMake| toInt(&a) *  &b |]
            _        -> err ErrFatal $ "Unknown quantifier: " <+> pretty quantifier


glueOp :: MonadConjure m => Text -> E -> E -> m E
glueOp quantifier a b = case quantifier of
            "forAll" -> return [eMake| &a /\ &b |]
            "exists" -> return [eMake| &a \/ &b |]
            "sum"    -> return [eMake| &a +  &b |]
            _        -> err ErrFatal $ "Unknown quantifier: " <+> pretty quantifier

