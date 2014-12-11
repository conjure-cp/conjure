{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Conjure.Prelude
    ( module X
    , stringToDoc
    , padRight, padLeft, padCenter
    , pairWithContents
    , withRest, withAfter, withBefore
    , T.Text, stringToText
    , sameLength
    , concatMapM
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
    , jsonOptions
    , Proxy(..)
    , MonadFail(..), failCheaply, na
    , allContexts, ascendants
    , headInf
    , paddedNum
    , dropExtension, dropExtensions
    , MonadLog(..), LogLevel(..)
    , runLoggerPipeIO, runLoggerIO, runLogger, ignoreLogs
    , logInfo, logWarn, logDebug, logDebugVerbose
    , histogram
    , ExceptT(..)
    , sh
    , scope
    , allFiles, allFilesWithSuffix
    , setRandomSeed, randomRIO
    ) where

import GHC.Err as X ( error )

-- basic data types
import Data.Bool as X ( Bool(..), (||), (&&), not, otherwise )
import Data.Int as X ( Int )
import GHC.Integer as X ( Integer )
import GHC.Exts as X ( Double )
import GHC.Real as X ( Fractional(..), Integral(..), fromIntegral, (^), Real(..) )
import GHC.Enum as X ( Enum(..) )
import Data.Char as X ( Char, toLower, isSpace )
import Data.String as X ( String, IsString(..) )

-- basic type classes
import Data.Eq as X ( Eq(..) )
import Data.Ord as X ( Ord(..), Ordering(..), comparing )
import Text.Show as X ( Show(..), showString, showParen )
import Text.Read as X ( Read(..), reads )
import GHC.Num as X ( Num(..) )

-- some more type classes
import GHC.Generics as X ( Generic )
import Data.Functor as X ( Functor(..) )
import Control.Applicative as X ( Applicative(..), (<$>), (<*), (*>), (<|>), many, some )
import qualified Control.Monad ( fail )
import Control.Monad as X ( Monad(return, (>>), (>>=)), MonadPlus(..), guard, void, mzero, msum, when, unless
                          , zipWithM, zipWithM_
                          , (<=<), (>=>), (=<<), foldM, ap, replicateM, liftM
                          , filterM, join
                          )
import Control.Monad.Trans.Class as X ( MonadTrans(lift) )

import Control.Monad.Identity       as X ( Identity, runIdentity )
import Control.Monad.Except         as X ( catchError )
import Control.Monad.IO.Class       as X ( MonadIO, liftIO )
import Control.Monad.State.Strict   as X ( MonadState, StateT, gets, modify, evalStateT, runStateT, evalState, runState )
import Control.Monad.Trans.Identity as X ( IdentityT, runIdentityT )
import Control.Monad.Trans.Maybe    as X ( MaybeT(..), runMaybeT )
import Control.Monad.Writer.Strict  as X ( MonadWriter(listen, tell), WriterT, runWriterT, execWriterT, runWriter )
import Control.Arrow             as X ( first, second, (***), (&&&) )
import Control.Category          as X ( (<<<), (>>>) )


import Data.Data         as X ( Data, Typeable )
import Data.Default      as X ( Default, def )
import Data.Either       as X ( Either(..), either, lefts, rights )
import Data.Function     as X ( id, const, flip, on, ($), (.) )
import Data.List         as X ( (\\), intercalate, intersperse, minimumBy, nub, nubBy
                              , group, groupBy, sort, sortBy, partition
                              , genericLength, genericIndex
                              , isSuffixOf, isPrefixOf, isInfixOf
                              , subsequences, transpose, elemIndex
                              , replicate, length
                              , (++), map, concat, null, filter, reverse, lookup, elem, unlines, words
                              , and, or, zipWith, concatMap, all, lines, notElem, foldr
                              , sum, product, unzip, zip, zip3, foldr1, foldl, any
                              , unzip3, repeat, dropWhile, unwords, intersect
                              , take, drop
                              , head, init, tail, last
                              )
import Data.List.Split   as X ( splitOn )
import Data.Maybe        as X ( Maybe(..), catMaybes, listToMaybe, fromMaybe, maybe, maybeToList, mapMaybe, isJust )
import Data.Monoid       as X ( Monoid, mempty, mappend, mconcat, Any(..) )
import Data.Tuple        as X ( fst, snd, swap, curry, uncurry )

import Data.Foldable     as X ( Foldable, mapM_, forM_, sequence_, fold, foldMap, toList, maximum, minimum )
import Data.Traversable  as X ( Traversable, mapM, forM, sequence )

import System.IO as X ( FilePath, IO, putStr, putStrLn, print, writeFile, getContents, getLine )

-- safe
import Safe as X ( at, atNote, readMay, readNote, headNote, fromJustNote )

-- hashable
import Data.Hashable as X ( Hashable(..), hash )

-- cereal
import Data.Serialize as X ( Serialize, encode, decode )
import qualified Data.Serialize

-- aeson
import Data.Aeson as X ( ToJSON(..), FromJSON(..) )
import qualified Data.Aeson.Types as JSON

-- QuickCheck
import Test.QuickCheck ( Gen )

-- parsec
import Text.Parsec ( ParsecT )

-- pretty
import Text.PrettyPrint as X
    ( Doc
    , (<>), (<+>), ($$)
    , hang, nest, punctuate 
    , hcat, vcat, fsep, hsep, sep
    )

-- uniplate
import Data.Generics.Uniplate.Data as X
    ( transform, transformBi
    , transformM, transformBiM
    , descend, descendM
    , descendBi, descendBiM
    , universe, universeBi
    , children
    )
import Data.Generics.Uniplate.Zipper as Zipper ( Zipper, down, right, up, hole )

-- pipes
import qualified Pipes

-- groom
import Text.Groom as X ( groom )

-- shelly
import Shelly ( Sh, shelly, print_stdout, print_stderr )

import System.Random ( StdGen, mkStdGen, setStdGen, randomRIO )

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

tracing :: Show a => String -> a -> a
tracing s a = trace ("tracing " ++ s ++ ": " ++ show a) a

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
withAfter :: [a] -> [(a,[a])]
withAfter [] = []
withAfter (x:xs) = (x,xs) : withAfter xs

-- generate a list yielding the elements of the input list in order in the fst component.
-- the snd component is all those elements to the left of fst.
withBefore :: [a] -> [(a,[a])]
withBefore = reverse . withAfter . reverse


sameLength :: [a] -> [b] -> Bool
sameLength [] [] = True
sameLength (_:xs) (_:ys) = sameLength xs ys
sameLength _ _ = False

concatMapM :: (Functor m, Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

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


jsonOptions :: JSON.Options
jsonOptions = JSON.defaultOptions
    { JSON.allNullaryToStringTag = True
    , JSON.omitNothingFields = True
    , JSON.sumEncoding = JSON.ObjectWithSingleField
    }


data Proxy a = Proxy


class (Functor m, Applicative m, Monad m) => MonadFail m where
    fail :: Doc -> m a

na :: MonadFail m => Doc -> m a
na message = fail ("N/A:" <+> message)

instance MonadFail Identity where
    fail = error . show

instance MonadFail Maybe where
    fail = const Nothing

instance MonadFail IO where
    fail = error . show

instance (a ~ Doc) => MonadFail (Either a) where
    fail = Left

instance MonadFail m => MonadFail (IdentityT m) where
    fail = lift . fail

instance (Functor m, Monad m) => MonadFail (MaybeT m) where
    fail = const $ MaybeT $ return Nothing

instance (Functor m, Monad m) => MonadFail (ExceptT m) where
    fail = ExceptT . return . Left

instance (Functor m, Monad m, MonadFail m) => MonadFail (StateT st m) where
    fail = lift . fail

instance MonadFail Gen where
    fail = Control.Monad.fail . show

instance MonadFail (ParsecT g l m) where
    fail = Control.Monad.fail . show

instance MonadFail m => MonadFail (LoggerT m) where
    fail = lift . fail

instance (MonadFail m, Monoid w) => MonadFail (WriterT w m) where
    fail = lift . fail

instance MonadFail m => MonadFail (Pipes.Proxy a b c d m) where
    fail = lift . fail

newtype ExceptT m a = ExceptT { runExceptT :: m (Either Doc a) }

instance (Functor m) => Functor (ExceptT m) where
    fmap f = ExceptT . fmap (fmap f) . runExceptT

instance (Functor m, Monad m) => Applicative (ExceptT m) where
    pure = return
    (<*>) = ap

instance (Monad m) => Monad (ExceptT m) where
    return a = ExceptT $ return (Right a)
    m >>= k = ExceptT $ do
        a <- runExceptT m
        case a of
            Left e -> return (Left e)
            Right x -> runExceptT (k x)
    fail = ExceptT . return . Left . stringToDoc

-- | "failCheaply: premature optimisation at its finest." - Oz
--   If you have a (MonadFail m => m a) action at hand which doesn't require anything else from the monad m,
--   it can be run in any monad that implements MonadFail.
--   Running it in a monad like IO will be a little bit more expensive though.
--   Why not run it in Either and raise the error in the outer monad instead?
--   Notice: this function cannot be eta-reduced.
failCheaply :: MonadFail m2 => (forall m . MonadFail m => m a) -> m2 a
failCheaply m = either fail return m


allContexts :: Data b => Zipper a b -> [Zipper a b]
allContexts z0 = concatMap subtreeOf (allSiblings z0)
    where
        -- the input has to be the left most
        allSiblings :: Zipper a b -> [Zipper a b]
        allSiblings z = z : maybe [] allSiblings (right z)

        subtreeOf :: Data b => Zipper a b -> [Zipper a b]
        subtreeOf z = z : maybe [] allContexts (down z)

ascendants :: Zipper a b -> [b]
ascendants z = hole z : maybe [] ascendants (Zipper.up z)


headInf :: [a] -> a
headInf (a:_) = a
headInf _ = error "End of infinite stream! Well done!"


paddedNum :: Show a => a -> String
paddedNum x = replicate (6 - length s) '0' ++ s
    where s = show x


dropExtension :: FilePath -> FilePath
dropExtension = intercalate "." . init . splitOn "."

dropExtensions :: FilePath -> FilePath
dropExtensions = head . splitOn "."


class (Functor m, Applicative m, Monad m) => MonadLog m where
    log :: LogLevel -> Doc -> m ()

data LogLevel
    = LogNone
    | LogInfo
    | LogWarn
    | LogDebug
    | LogDebugVerbose
    deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Default LogLevel where def = LogNone

logInfo :: MonadLog m => Doc -> m ()
logInfo = log LogInfo

logWarn :: MonadLog m => Doc -> m ()
logWarn = log LogWarn

logDebug :: MonadLog m => Doc -> m ()
logDebug = log LogDebug

logDebugVerbose :: MonadLog m => Doc -> m ()
logDebugVerbose = log LogDebugVerbose

instance MonadLog m => MonadLog (StateT st m) where
    log l m = lift (log l m)

instance MonadLog m => MonadLog (ExceptT m) where
    log l m = log l m >> ExceptT (return (Right ()))

instance (Applicative m, Monad m) => MonadLog (IdentityT m) where
    log _ _ = return ()

instance Monad m => MonadLog (Pipes.Proxy a b () (Either (LogLevel, Doc) d) m) where
    log l m = Pipes.yield (Left (l,m))

ignoreLogs :: Monad m => IdentityT m a -> m a
ignoreLogs = runIdentityT

newtype LoggerT m a = LoggerT (WriterT [(LogLevel, Doc)] m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance (Applicative m, Monad m) => MonadLog (LoggerT m) where
    log lvl msg = LoggerT $ tell [(lvl, msg)]

runLogger :: Monad m => LogLevel -> LoggerT m a -> m (a, [Doc])
runLogger l (LoggerT ma) = do
    (a, logs) <- runWriterT ma
    return (a, [ msg | (lvl, msg) <- logs , lvl <= l ])

runLoggerIO :: MonadIO m => LogLevel -> LoggerT m a -> m a
runLoggerIO l logger = do
    (a, logs) <- runLogger l logger
    liftIO $ print (vcat logs)
    return a

runLoggerPipeIO :: MonadIO m => LogLevel -> Pipes.Producer (Either (LogLevel, Doc) a) m r -> m r
runLoggerPipeIO l logger = Pipes.runEffect $ Pipes.for logger each
    where
        each (Left (lvl, msg)) = when (lvl <= l)
            (liftIO $ putStrLn $ Pr.renderStyle (Pr.style { Pr.lineLength = 200 }) msg)
        each _ = return ()

histogram :: Ord a => [a] -> [(a,Int)]
histogram = map (head &&& length) . group . sort

sh :: Sh a -> IO a
sh = shelly . print_stdout False . print_stderr False

scope :: MonadState st m => m a -> m a
scope ma = do
    st <- gets id
    a <- ma
    modify (const st)
    return a

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

setRandomSeed :: Int -> IO ()
setRandomSeed = setStdGen . mkStdGen
