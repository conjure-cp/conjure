module Test.All ( allTests ) where


import Control.Applicative
import Test.HUnit ( Test, test )

import TestUtils ( runTest )
import Test.Language.EssenceParsers       as Pa  ( allTests )
import Test.Language.EssencePrinters      as Pr  ( allTests )
import Test.Language.EssenceParsePrintIso as Iso ( allTests )


allTests :: IO Test
allTests = test <$> sequence [ Pa.allTests
                             , Pr.allTests
                             , Iso.allTests
                             ]
