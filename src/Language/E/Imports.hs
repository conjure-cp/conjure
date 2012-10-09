-- mostly for library dependencies.
-- defines a specialised prelude. kind-of.
-- things in Language.E.* should import this.
-- also see "Language.E", which imports and re-exports this module.

module Language.E.Imports
    ( module X
    , stringToDoc
    , setEq
    , padRight, padLeft, padCenter
    , pairWithContents
    , withRest, withRestToR, withRestToL
    , T.Text
    , ppShow, ppPrint
    , replace, replaceAll
    , sameLength
    , concatMapM
    , parMapM
    , allFiles, allFilesWithSuffix
    ) where

import Control.Applicative       as X ( Applicative(..), (<$>), (<*), (*>) )
import Control.Arrow             as X ( first, second )

import Control.Monad             as X ( MonadPlus, void, mzero, msum, when, unless, zipWithM, (<=<), (>=>), foldM, ap, replicateM )
import Control.Monad.Trans.Class as X ( MonadTrans, lift )
import Control.Monad.Identity    as X ( Identity, runIdentity )
import Control.Monad.Reader      as X ( MonadReader(..) )
import Control.Monad.Writer      as X ( MonadWriter(..), WriterT, runWriterT, execWriterT )
import Control.Monad.Error       as X ( MonadError(..), ErrorT, runErrorT )
import Control.Monad.IO.Class    as X ( MonadIO, liftIO )
import Control.Monad.Trans.Maybe as X ( MaybeT(..), runMaybeT )


import Data.Default      as X ( Default, def )
import Data.Either       as X ( lefts, rights )
import Data.Foldable     as X ( forM_, fold, foldMap, toList )
import Data.Function     as X ( on )
import Data.List         as X ( (\\), intercalate, intersperse, minimumBy, nub, groupBy, sortBy, partition, genericLength, genericIndex, isSuffixOf )
import Data.List.Split   as X ( splitOn )
import Data.Maybe        as X ( catMaybes, listToMaybe, maybeToList, mapMaybe, isJust )
import Data.Monoid       as X ( Monoid, mempty, mappend, mconcat, Any(..) )
import Data.Ord          as X ( comparing )
import Data.Traversable  as X ( forM )

import Data.Generics.Uniplate.Data as X ( Uniplate, universe, transform )

import Text.PrettyPrint as X ( Doc, nest, punctuate, sep, vcat, (<+>), ($$) )

import Debug.Trace as X ( trace )

import Text.Show.Pretty ( ppDoc )

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.PrettyPrint as Pr
import qualified Data.Set as S

import Control.Concurrent ( getNumCapabilities )
import Control.Concurrent.ParallelIO.Local ( withPool, parallel )
import System.Directory ( getDirectoryContents )
import System.FilePath ( (</>) )


stringToDoc :: String -> Doc
stringToDoc = Pr.text

setEq :: Ord a => [a] -> [a] -> Bool
setEq xs ys = S.fromList xs == S.fromList ys

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

ppShow :: Show a => a -> String
ppShow = Pr.renderStyle Pr.style { Pr.lineLength = 200 } . ppDoc

ppPrint :: Show a => a -> IO ()
ppPrint = putStrLn . ppShow


replace :: (Uniplate a, Eq a) => a -> a -> a -> a
replace old new = transform $ \ i -> if i == old then new else i

replaceAll :: (Uniplate a, Eq a) => [(a,a)] -> a -> a
replaceAll [] x = x
replaceAll ((old,new):rest) x = replaceAll rest $ replace old new x

sameLength :: [a] -> [b] -> Bool
sameLength [] [] = True
sameLength (_:xs) (_:ys) = sameLength xs ys
sameLength _ _ = False

concatMapM :: (Functor m, Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = fmap concat $ mapM f xs

parMapM :: (a -> IO b) -> [a] -> IO [b]
parMapM f xs = do
   n <- getNumCapabilities
   withPool n $ \pool -> parallel pool (map f xs)

allFiles :: FilePath -> IO [FilePath]
allFiles x = do
    let dots i = not $ or [ i == "." , i == ".." ]
    ys' <- getDirectoryContents x `catchError` (const $ return [])
    let ys = filter dots ys'
    if null ys
        then return [x]
        else (x :) <$> concatMapM allFiles (map (x </>) ys)

allFilesWithSuffix :: String -> FilePath -> IO [FilePath]
allFilesWithSuffix suffix fp = filter (suffix `isSuffixOf`) <$> allFiles fp

