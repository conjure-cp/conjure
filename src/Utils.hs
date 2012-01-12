module Utils
    ( allValues
    , maybeRead
    , ppShow, ppPrint
    , strip
    , runRWSE, runRWSET
    , fst3, snd3, thd3
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

runRWSET :: Monad m => r -> s -> ErrorT e (RWST r w s m) a -> m (Either e a, w)
runRWSET r s comp = evalRWST (runErrorT comp) r s


fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c
