module Test.Language.Unit.Random where


import Control.Applicative
import Test.HUnit ( Test, test )
import Test.QuickCheck ( Positive(..) )

import Language.Essence ( Expr(..), Op(..) )
import Test.Language.Unit.Common
import TestUtils ( quickTest )


allTests :: IO Test
allTests = test . concat <$> sequence
                [ quickTest propInteger
                , quickTest propIntegerNegative
                ]


propInteger :: Positive Integer -> Test
propInteger (Positive i) =
    cmdParsePrint
        (show i)
        (show (ValueInteger i))

propIntegerNegative :: Positive Integer -> Test
propIntegerNegative (Positive j) = let i = -j in
    cmdParsePrint
        (show i)
        $ show (GenericNode Negate [ValueInteger (abs i)])
