module Test.Language.OnFile.RuleRepr where

import Test.HUnit ( Test )

import Language.Essence ( RuleRepr, reprPrologueBindings )
import Language.EssenceParsers ( pRuleRepr )
import Language.EssencePrinters ( prRuleRepr )
import Language.EssenceTypes ( typeCheckRuleRepr )
import Test.Language.OnFile.Common


allTests :: [String] -> Test
allTests = mkAllTests "RuleRepr" f
    where
        f filename = testOne (pRuleRepr filename)
                             prRuleRepr
                             typeCheckRuleRepr
                             reprPrologueBindings
                             filename
