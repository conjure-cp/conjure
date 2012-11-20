{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Language.E.CompE where

import Stuff.Funky.FunkySingle
import Stuff.Funky.FunkyMulti
import Stuff.NamedLog

import Language.E.Imports
import Language.E.Definition
import Language.E.Pretty

import qualified Data.Set as S
import qualified Data.Map as M


class ( Functor m
      , Applicative m
      , Monad m
      , MonadState ConjureState m
      , MonadError ConjureError m
      ) => MonadConjure m where
    type ResultF m a
    runFunky :: ConjureState -> m a -> ResultF m (Either ConjureError a, ConjureState)


instance MonadConjure (FunkySingle ConjureState ConjureError Identity) where
    type ResultF      (FunkySingle ConjureState ConjureError Identity) a = a
    runFunky st ma = runIdentity $ runFunkySingle st ma

instance MonadConjure (FunkyMulti  () ConjureState ConjureError Identity) where
    type ResultF      (FunkyMulti  () ConjureState ConjureError Identity) a = [a]
    runFunky st ma = fst $ runIdentity $ runFunkyMulti () st ma


instance MonadConjure (FunkySingle ConjureState ConjureError IO) where
    type ResultF    (FunkySingle ConjureState ConjureError IO) a = IO a
    runFunky = runFunkySingle

instance MonadConjure (FunkyMulti  () ConjureState ConjureError IO) where
    type ResultF      (FunkyMulti  () ConjureState ConjureError IO) a = IO [a]
    runFunky st ma = liftM fst $ runFunkyMulti () st ma


runCompE
    :: String
    -> FunkyMulti () ConjureState ConjureError Identity a
    -> [(Either Doc a, LogTree)]
runCompE d ma = map (afterCompERun d) (fst $ runIdentity $ runFunkyMulti () def ma)


runCompEIO
    :: String
    -> FunkyMulti () ConjureState ConjureError IO a
    -> IO [(Either Doc a, LogTree)]
runCompEIO d ma = map (afterCompERun d) . fst <$> runFunkyMulti () def ma


runCompESingle
    :: String
    -> FunkySingle ConjureState ConjureError Identity a
    -> (Either Doc a, LogTree)
runCompESingle d ma = afterCompERun d $ runIdentity $ runFunkySingle def ma


runCompEIOSingle
    :: String
    -> FunkySingle ConjureState ConjureError IO a
    -> IO (Either Doc a, LogTree)
runCompEIOSingle d ma = afterCompERun d <$> runFunkySingle def ma


afterCompERun
    :: String
    -> (Either ConjureError a, ConjureState)
    -> (Either Doc a, LogTree)
afterCompERun d = first (either (toError d) Right) . second localLogs


handleInIO
    :: [(Either Doc a, LogTree)]
    -> IO [a]
handleInIO = mapM handleInIOSingle


handleInIOSingle
    :: (Either Doc a, LogTree)
    -> IO a
handleInIOSingle (mx, logs) = do
    printLogs logs
    case mx of
        Left  x -> error $ renderPretty x
        Right x -> return x

toError :: String -> ConjureError -> Either Doc a
toError msg
    = Left
    . prettyError (pretty $ "Error in phase: " ++ msg)



-- errors

type ConjureError = (ErrEnum, Doc, Maybe Spec)

data ErrEnum = ErrFatal        -- means execution cannot continue.
    deriving (Eq, Show)

err :: MonadConjure m => ErrEnum -> Doc -> m a
err e d = do
    sp <- gets lastSpec
    throwError (e,d,sp)

prettyError :: Doc -> ConjureError -> Doc
prettyError msg (_, d, Nothing) = vcat [msg, nest 4 d, ""]
prettyError msg (_, d, Just sp) = vcat [msg, nest 4 d, nest 4 (pretty sp), ""] 

recordSpec :: MonadConjure m => Spec -> m Spec
recordSpec sp = do
    modify $ \ st -> st { lastSpec = Just sp }
    return sp


-- state

data ConjureState = ConjureState
        { binders       :: [Binder]
        , uniqueNameInt :: Integer
        , representationConfig :: M.Map String [RuleReprResult]
        , representationLog :: [ ( String   -- original name
                                 , String   -- representation name
                                 , E        -- original full declaration
                                 , E        -- new domain
                                 ) ]
        , structuralConsLog :: [E]
        , lastSpec :: Maybe Spec  -- record the spec after changes, to report in case of an error.
        , localLogs :: LogTree
        , allNamesPreConjure :: S.Set String  -- all identifiers used in the spec, pre conjure. to avoid name clashes.
        }

data Binder = Binder String E
    deriving (Show)

instance Default ConjureState where
    def = ConjureState def 1 def def def def def def


mkLog :: MonadConjure m => String -> Doc -> m ()
mkLog nm doc = case buildLog nm doc of
    Nothing -> return ()
    Just l  -> modify $ \ st -> st {
        localLogs = LTMultiple (localLogs st) (LTSingle l)
        }

addBinder :: MonadConjure m => String -> E -> m ()
addBinder nm val = modify $ \ st -> st { binders = Binder nm val : binders st }

lookupBinder :: MonadConjure m => String -> MaybeT m E
lookupBinder nm = do
    bs <- lift $ gets binders
    case listToMaybe [ x | Binder nm' x <- bs, nm == nm' ] of
        Nothing -> mzero
        Just x  -> return x

nextUniqueName :: MonadConjure m => m String
nextUniqueName = do
    i <- gets uniqueNameInt
    modify $ \ st -> st { uniqueNameInt = i + 1 }
    let nm = "v__" ++ show i
    nms <- gets allNamesPreConjure
    if nm `S.member` nms
        then nextUniqueName
        else return nm


makeIdempotent :: Monad m => (a -> m (a,Bool)) -> a -> m a
makeIdempotent f x = do
    (y,flag) <- f x
    if flag
        then makeIdempotent f y
        else return y



