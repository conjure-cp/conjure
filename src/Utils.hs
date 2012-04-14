{-# LANGUAGE TupleSections #-}

module Utils
    ( allValues
    , maybeRead
    , ppShow, ppPrint, getCh
    , strip
    , runRWSE, runRWSET, rwst
    , fst3, snd3, thd3
    , fromJust, padLeft, padRight
    , applyAll, applyAllM
    , allPairs
    , safeStr
    , mapButLast, matchingOuterParens
    , equalLengths
    , concatMapM, allEq
    , isLeft, isRight
    ) where

import Control.Applicative
import Control.Monad ( foldM )
import Control.Monad.Error ( ErrorT, runErrorT )
import Control.Monad.RWS ( RWS, evalRWS, RWST(..), evalRWST )
import Data.Maybe ( listToMaybe )
import System.IO ( hSetEcho, stdin )
import Text.PrettyPrint ( lineLength, renderStyle, style )
import Text.Show.Pretty ( ppDoc )


isLeft :: Either a b -> Bool
isLeft Left {} = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight Right {} = True
isRight _ = False

concatMapM :: (Applicative m, Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

equalLengths :: [a] -> [b] -> Bool
equalLengths [] [] = True
equalLengths (_:a) (_:b) = equalLengths a b
equalLengths _ _ = False

mapButLast :: (a -> a) -> [a] -> [a]
mapButLast _ [ ]    = []
mapButLast _ [x]    = [x]
mapButLast f (x:xs) = f x : mapButLast f xs

fromJust :: String -> Maybe a -> a
fromJust msg Nothing  = error msg
fromJust _   (Just a) = a

padLeft :: a -> Int -> [a] -> [a]
padLeft pre n xs = replicate (n - length xs) pre ++ xs

padRight :: a -> Int -> [a] -> [a]
padRight pre n xs = xs ++ replicate (n - length xs) pre


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


getCh :: IO Char
getCh  = do
    hSetEcho stdin False
    c <- getChar
    hSetEcho stdin True
    return c


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


-- apply all those functions successively.
applyAll :: a -> [a -> a] -> a
applyAll = foldl (\ t f -> f t)
-- applyAll x []     = x
-- applyAll x (f:fs) = applyAll (f x) fs

-- apply all those functions successively. in an M
applyAllM :: (Monad m) => a -> [a -> m a] -> m a
applyAllM = foldM (\ t f -> f t)
-- applyAllM x []     = return x
-- applyAllM x (f:fs) = do y <- f x; applyAllM y fs

allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (x:xs) = map (x,) xs ++ allPairs xs

safeStr :: String -> String
safeStr = map (\ ch -> if ch `elem` "/.()" then '_' else ch)


rwst :: (r -> s -> m (a, s, w)) -> RWST r w s m a
rwst = RWST


allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (x:xs) = all (x==) xs

-- check if a String is wrapped in parens. any parens inside should match-up.
-- matchingOuterParens "(foo)" = True
-- matchingOuterParens "(foo)=(bar)" = False
-- matchingOuterParens "((foo)=(bar))" = True
matchingOuterParens :: String -> Bool
matchingOuterParens ('(':xs) = check [] xs
    where
        check []    ")"    = True
        check stack ('(':as) = check (() : stack) as
        check []    (')':_ ) = False -- error "closing parens with no matching openning parens"
        check stack (')':as) = check (tail stack) as
        check _     []       = False
        check stack (_  :as) = check stack as
matchingOuterParens _ = True
