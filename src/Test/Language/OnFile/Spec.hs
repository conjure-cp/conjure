module Test.Language.OnFile.Spec where

import Test.HUnit ( Test )

import Language.Essence ( Spec, topLevelBindings )
import Language.EssenceParsers ( pSpec )
import Language.EssencePrinters ( prSpec )
import Language.EssenceTypes ( typeCheckSpec )
import Test.Language.OnFile.Common


allTests :: [String] -> Test
allTests = mkAllTests "RuleRepr"
    $ testOne pSpec prSpec typeCheckSpec topLevelBindings
