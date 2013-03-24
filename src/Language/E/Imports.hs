{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- mostly for library dependencies.
-- defines a specialised prelude. kind-of.
-- things in Language.E.* should import this.
-- also see "Language.E", which imports and re-exports this module.

module Language.E.Imports
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
    , trace, tracing
    , allCombinations
    , sortOn, sortNub
    , maybeRead
    , padShowInt
    , decodeFromFile
    , RandomM(..)
    , bug, userErr
    , headNote
    ) where

import Control.Applicative       as X ( Applicative(..), (<$>), (<$), (<*), (*>), (<|>), many, some )
import Control.Arrow             as X ( first, second, (***) )
import Control.Category          as X ( (<<<), (>>>) )
import Control.Monad             as X ( MonadPlus, void, mzero, msum, when, unless, zipWithM, (<=<), (>=>), foldM, ap, replicateM, liftM )
import Control.Monad.Trans.Class as X ( MonadTrans(lift) )

import Control.Monad.Error          as X ( MonadError(throwError, catchError), ErrorT, runErrorT, Error(..) )
import Control.Monad.Identity       as X ( Identity, runIdentity )
import Control.Monad.IO.Class       as X ( MonadIO, liftIO )
import Control.Monad.State.Strict   as X ( MonadState(get, put), gets, modify, evalStateT, runStateT )
import Control.Monad.Trans.Identity as X ( runIdentityT )
import Control.Monad.Trans.Maybe    as X ( MaybeT, runMaybeT )
import Control.Monad.Writer.Strict  as X ( MonadWriter(listen, tell), WriterT, runWriterT, execWriterT, runWriter )

import Data.Default      as X ( Default, def )
import Data.Either       as X ( lefts, rights )
import Data.Foldable     as X ( forM_, fold, foldMap, toList )
import Data.Function     as X ( on )
import Data.List         as X ( (\\), intercalate, intersperse, minimumBy, nub, nubBy, group, groupBy, sort, sortBy, partition, genericLength, genericIndex, isSuffixOf, isPrefixOf, subsequences )
import Data.List.Split   as X ( splitOn )
import Data.Maybe        as X ( catMaybes, listToMaybe, fromMaybe, maybe, maybeToList, mapMaybe, isJust )
import Data.Monoid       as X ( Monoid, mempty, mappend, mconcat, Any(..) )
import Data.Ord          as X ( comparing )
import Data.Traversable  as X ( forM )
import Data.Tuple        as X ( swap )

import Control.DeepSeq          as X ( NFData(..), force, ($!!) )
import Control.DeepSeq.Generics as X ( genericRnf )

import Data.Hashable         as X ( Hashable(..), hash )

import Data.Serialize as X ( Serialize, encode, decode )
import qualified Data.Serialize

import Text.PrettyPrint as X ( Doc, nest, punctuate, sep, hsep, vcat, (<+>), ($$) )

import System.Random as X ( StdGen, getStdGen, randomR )

import qualified Data.ByteString as ByteString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.PrettyPrint as Pr

import qualified Data.Set as S

import System.Directory ( getDirectoryContents )
import System.FilePath ( (</>) )
import System.CPUTime ( getCPUTime )

import RepositoryVersion ( repositoryVersion )
import Stuff.Pretty


#ifdef TRACELOGS
import qualified Debug.Trace ( trace )
trace :: String -> a -> a
trace = Debug.Trace.trace
#else
trace :: String -> a -> a
trace = const id
#endif

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
concatMapM f xs = fmap concat $ mapM f xs

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

instance Error Doc where strMsg = Pr.text

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


-- call this function instead of "error"
-- the String argument is only printed if compiled with --trace-logs
bug :: Doc -> a
bug _message = error $ unlines
    [ "This should never happen, sorry!"
    , ""
    , "Please report a bug."
    , "Conjure is actively maintained, we will get back to you as soon as possible."
    , "You can help us by providing a minimal failing example."
    , "Also include repository version for this build: " ++ repositoryVersion
    , ""
    , "Issue tracker: http://bitbucket.org/stacs_cp/conjure-public/issues"
#ifdef TRACELOGS
    , "", "" , renderPretty _message
#endif
    ]


headNote :: Doc -> [a] -> a
headNote msg [] = bug msg
headNote _   (x:_) = x


-- call this function instead of "error"
-- in case of a user error.
-- parsing, type checking errors are of this kind.
userErr :: Doc -> a
userErr = error . renderPretty


