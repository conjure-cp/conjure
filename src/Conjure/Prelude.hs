{-# OPTIONS_GHC -fno-warn-orphans #-}

module Conjure.Prelude
    ( module X
    , stringToDoc
    , padRight, padLeft, padCenter
    , pairWithContents
    , withRest, withRestToR, withRestToL
    , T.Text, stringToText
    , sameLength
    , concatMapM
    , allFiles, allFilesWithSuffix
    , timedIO
    , isLeft, isRight
    , tracing
    , allCombinations
    , sortOn, sortNub
    , maybeRead
    , padShowInt
    , decodeFromFile
    , RandomM(..)
    , fst3, snd3, thd3
    , (|>)
    , allNats
    ) where

import GHC.Err as X ( error )

-- basic data types
import Data.Bool as X ( Bool(..), (||), (&&), not, otherwise )
import Data.Int as X ( Int )
import GHC.Integer as X ( Integer )
import GHC.Exts as X ( Double )
import GHC.Real as X ( Fractional(..), Integral(..), fromIntegral, (^), Real(..) )
import GHC.Enum as X ( Enum(..) )
import Data.Char as X ( Char, toLower )
import Data.String as X ( String, IsString(..) )

-- basic type classes
import Data.Eq as X ( Eq(..) )
import Data.Ord as X ( Ord(..), Ordering(..), comparing )
import Text.Show as X ( Show(..), showString, showParen )
import Text.Read as X ( Read(..), reads )
import GHC.Num as X ( Num(..) )

-- some more type classes
import Data.Functor as X ( Functor(..) )
import Control.Applicative as X ( Applicative(..), (<$>), (<*), (*>), (<|>), many, some )
import Control.Monad as X ( Monad(..), MonadPlus(..), guard, void, mzero, msum, when, unless, zipWithM
                          , (<=<), (>=>), (=<<), foldM, ap, replicateM, liftM, sequence
                          , filterM
                          )
import Control.Monad.Trans.Class as X ( MonadTrans(lift) )

import Control.Monad.Except         as X ( MonadError(throwError, catchError) )
import Control.Monad.Trans.Except   as X ( runExceptT )
import Control.Monad.Identity       as X ( Identity, runIdentity )
import Control.Monad.IO.Class       as X ( MonadIO, liftIO )
import Control.Monad.State.Strict   as X ( MonadState, gets, modify, evalStateT, runStateT, runState )
import Control.Monad.Trans.Identity as X ( runIdentityT )
import Control.Monad.Trans.Maybe    as X ( MaybeT, runMaybeT )
import Control.Monad.Writer.Strict  as X ( MonadWriter(listen, tell), WriterT, runWriterT, execWriterT, runWriter )
import Control.Arrow             as X ( first, second, (***) )
import Control.Category          as X ( (<<<), (>>>) )


import Data.Data         as X ( Data, Typeable )
import Data.Default      as X ( Default, def )
import Data.Either       as X ( Either(..), either, lefts, rights )
import Data.Foldable     as X ( forM_, fold, foldMap, toList, mapM_ )
import Data.Function     as X ( id, const, flip, on, ($), (.) )
import Data.List         as X ( (\\), intercalate, intersperse, minimumBy, nub, nubBy
                              , group, groupBy, sort, sortBy, partition
                              , genericLength, genericIndex
                              , isSuffixOf, isPrefixOf
                              , subsequences, transpose, findIndex
                              , replicate, length
                              , (++), map, concat, null, filter, reverse, lookup, elem, unlines, words, head
                              , init, and, or, zipWith, maximum, concatMap, all, lines, notElem, foldr
                              , sum, product, unzip, zip, zip3, take, (!!), foldr1, foldl, drop, any, tail
                              , unzip3, repeat, dropWhile
                              )
import Data.List.Split   as X ( splitOn )
import Data.Maybe        as X ( Maybe(..), catMaybes, listToMaybe, fromMaybe, maybe, maybeToList, mapMaybe, isJust )
import Data.Monoid       as X ( Monoid, mempty, mappend, mconcat, Any(..) )
import Data.Traversable  as X ( mapM, forM )
import Data.Tuple        as X ( fst, snd, swap, uncurry )

import System.IO as X ( FilePath, IO, putStr, putStrLn, print, writeFile, getContents )

-- hashable
import Data.Hashable as X ( Hashable(..), hash )

-- cereal
import Data.Serialize as X ( Serialize, encode, decode )
import qualified Data.Serialize

-- aeson
import Data.Aeson as X ( toJSON )

import Text.PrettyPrint as X ( Doc, nest, punctuate, sep, fsep, hsep, vcat, (<+>), ($$) )

import System.Random as X ( StdGen, getStdGen, randomR )

import qualified Data.ByteString as ByteString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.PrettyPrint as Pr

import qualified Data.Set as S

import System.Directory as X
    ( getDirectoryContents, doesDirectoryExist, doesFileExist
    , createDirectoryIfMissing, removeDirectoryRecursive
    )
import System.Environment as X ( getArgs )
import System.FilePath as X ( (</>) )
import System.CPUTime ( getCPUTime )

import Debug.Trace as X ( trace )

tracing :: Show a => a -> a
tracing a = trace (show a) a

stringToText :: String -> T.Text
stringToText = T.pack

stringToDoc :: String -> Doc
stringToDoc = Pr.text

padRight :: Int -> Char -> String -> String
padRight n ch s = s ++ replicate (n - length s) ch

padLeft :: Int -> Char -> String -> String
padLeft n ch s = replicate (n - length s) ch ++ s

padCenter :: Int -> Char -> String -> String
padCenter n ch s = replicate (div diff 2) ch ++ s ++ replicate (diff - div diff 2) ch
    where
        diff = n - length s

pairWithContents :: FilePath -> IO (FilePath, T.Text)
pairWithContents fp = do
    con <- T.readFile fp
    return (fp,con)

-- the fst component: generate a list yielding the elements of the input list in order
-- the snd component: is all those elements except the fst.
withRest :: [a] -> [(a,[a])]
withRest [] = []
withRest (x:xs) = (x,xs) : map (second (x:)) (withRest xs)

-- generate a list yielding the elements of the input list in order in the fst component.
-- the snd component is all those elements to the right of fst.
withRestToR :: [a] -> [(a,[a])]
withRestToR [] = []
withRestToR (x:xs) = (x,xs) : withRestToR xs

-- generate a list yielding the elements of the input list in order in the fst component.
-- the snd component is all those elements to the left of fst.
withRestToL :: [a] -> [(a,[a])]
withRestToL = reverse . withRestToR . reverse


sameLength :: [a] -> [b] -> Bool
sameLength [] [] = True
sameLength (_:xs) (_:ys) = sameLength xs ys
sameLength _ _ = False

concatMapM :: (Functor m, Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

allFiles :: FilePath -> IO [FilePath]
allFiles x = do
    let dots i = not ( i == "." || i == ".." )
    ys' <- getDirectoryContents x `catchError` const (return [])
    let ys = filter dots ys'
    if null ys
        then return [x]
        else (x :) <$> concatMapM allFiles (map (x </>) ys)

allFilesWithSuffix :: String -> FilePath -> IO [FilePath]
allFilesWithSuffix suffix fp = filter (suffix `isSuffixOf`) <$> allFiles fp

timedIO :: IO a -> IO (a, Double)
timedIO io = do
    start <- getCPUTime
    a <- io
    end   <- getCPUTime
    let diff = fromIntegral (end - start) / ((10 :: Double) ^ (12 :: Int))
    return (a, diff)

isLeft :: Either a b -> Bool
isLeft (Left {}) = True
isLeft _         = False

isRight :: Either a b -> Bool
isRight (Right {}) = True
isRight _          = False

allCombinations :: [(a,[b])] -> [[(a,b)]]
allCombinations [] = [[]]
allCombinations ((x,ys):qs) = concat [ [ (x,y) : ws | y <- ys ] | ws <- allCombinations qs ]

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (comparing f)

sortNub :: Ord a => [a] -> [a]
sortNub = S.toList . S.fromList


instance Serialize T.Text where
    put = Data.Serialize.put . T.unpack
    get = T.pack <$> Data.Serialize.get

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

padShowInt :: Show a => Int -> a -> String
padShowInt n i = let s = show i in replicate (n - length s) '0' ++ s

decodeFromFile :: Serialize a => FilePath -> IO a
decodeFromFile path = do
    con <- ByteString.readFile path
    either error return (decode con)

class Monad m => RandomM m where
    get_stdgen :: m StdGen
    set_stdgen :: StdGen -> m ()


fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c

instance Eq Doc where
    (==) = (==) `on` show

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

allNats :: [Int]
allNats = [1..]

