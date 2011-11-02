module Utils
    ( allValues
    , maybeRead
    , ppPrint
    , ppShow
    , strip
    ) where

import Data.Maybe ( listToMaybe )
import Text.Show.Pretty ( ppShow)

strip :: String -> String
strip = reverse . go . reverse . go
    where go = dropWhile isWhiteSpace
          isWhiteSpace = (`elem` " \n\t")


maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads


allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..maxBound]


ppPrint :: Show a => a -> IO ()
ppPrint = putStrLn . ppShow


-- arbitraryIdentifier :: Gen String
-- arbitraryIdentifier = do
--     let ch = elements $ ['a'..'z'] ++ ['A'..'Z']
--     s <- choose (2, 8 :: Int)
--     replicateM s ch

