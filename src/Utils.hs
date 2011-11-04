module Utils
    ( allValues
    , maybeRead
    , ppShow, ppPrint
    , strip
    ) where

import Data.Maybe ( listToMaybe )
import Text.PrettyPrint ( lineLength, renderStyle, style )
import Text.Show.Pretty ( ppDoc )

strip :: String -> String
strip = reverse . go . reverse . go
    where go = dropWhile isWhiteSpace
          isWhiteSpace = (`elem` " \n\t")


maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads


allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..maxBound]


ppShow :: Show a => a -> String
ppShow = renderStyle style { lineLength = 200 } . ppDoc

ppPrint :: Show a => a -> IO ()
ppPrint = putStrLn . ppShow


-- arbitraryIdentifier :: Gen String
-- arbitraryIdentifier = do
--     let ch = elements $ ['a'..'z'] ++ ['A'..'Z']
--     s <- choose (2, 8 :: Int)
--     replicateM s ch

