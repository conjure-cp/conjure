{-# LANGUAGE TemplateHaskell #-}

module Test.Language.EssenceParsePrintRandom where


import Control.Applicative
import Test.HUnit ( Test )
import Test.QuickCheck ( Positive(..) )

import Language.Essence ( Expr(..), Op(..) )
import Test.Language.EssenceParsePrintCommon ( parsePrint )
import TestUtils ( quickTest )


allTests :: IO [Test]
allTests = concat <$> sequence
                [ quickTest propInteger
                , quickTest propIntegerNegative
                ]


propInteger :: Positive Integer -> Test
propInteger (Positive i) =
    parsePrint
        (show i)
        (show (ValueInteger i))

propIntegerNegative :: Positive Integer -> Test
propIntegerNegative (Positive j) = let i = -j in
    parsePrint
        (show i)
        $ show (GenericNode Negate [ValueInteger (abs i)])
