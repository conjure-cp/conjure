module Utils
    ( allValues
    , maybeRead
    , ppShow, ppPrint
    , strip
    , runRWSE
    ) where

import Control.Monad.Error -- ( MonadError, runErrorT )
import Control.Monad.RWS -- ( MonadReader, MonadWriter, MonadState, evalRWS )
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


runRWSE :: r -> s -> ErrorT e (RWS r w s) a -> (Either e a, w)
runRWSE r s comp = evalRWS (runErrorT comp) r s
