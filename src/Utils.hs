module Utils where

import Data.Maybe ( listToMaybe )

strip :: String -> String
strip = reverse . go . reverse . go
    where go = dropWhile isWhiteSpace
          isWhiteSpace = (`elem` " \n\t")


maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads


-- arbitraryIdentifier :: Gen String
-- arbitraryIdentifier = do
--     let ch = elements $ ['a'..'z'] ++ ['A'..'Z']
--     s <- choose (2, 8 :: Int)
--     replicateM s ch


-- allValues :: (Bounded a, Enum a) => [a]
-- allValues = [minBound..maxBound]
