module Test.Language.OnFile.Spec where

import Test.HUnit ( Test )

import Language.Essence ( Spec, topLevelBindings )
import Language.EssenceParsers ( pSpec )
import Language.EssencePrinters ( prSpec )
import Language.EssenceTypes ( typeCheckSpec )
import Test.Language.OnFile.Common
import Utils ( ppShow )


allTests :: [String] -> Test
allTests = mkAllTests "RuleRepr"
    $ testOne pSpec prSpec (ppShow . rmKnownQuans) typeCheckSpec topLevelBindings


rmKnownQuans :: Spec -> Spec
rmKnownQuans sp = sp { topLevelBindings = [ b
                                          | b@(_,nm,_) <- topLevelBindings sp
                                          , nm `notElem` ["forall", "exists", "sum"]
                                          ]
                     }
