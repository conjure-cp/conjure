module Test.Utils where

import Control.Applicative
import Test.HUnit ( Test, test )

import TestUtils ( quickTestBool )
import Utils ( safeStr )


allTests :: IO Test
allTests = test . concat <$> sequence
                [ quickTestBool "propSafeStrLength" propSafeStrLength
                , quickTestBool "propSafeStrNoDots" propSafeStrNoDots
                ]


propSafeStrLength :: String -> Bool
propSafeStrLength s = length s == length (safeStr s)

propSafeStrNoDots :: String -> Bool
propSafeStrNoDots s = all (/='.') (safeStr s)
