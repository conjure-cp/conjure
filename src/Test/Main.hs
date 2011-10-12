module Main where


import Control.Applicative
import Test.HUnit ( test )

import TestUtils ( runTest )
import Test.Language.EssenceParsers       as Pa  ( allTests )
import Test.Language.EssencePrinters      as Pr  ( allTests )
import Test.Language.EssenceParsePrintIso as Iso ( allTests )


main :: IO ()
main = runTest $ test <$> sequence [ Pa.allTests
                                   , Pr.allTests
                                   , Iso.allTests
                                   ]
