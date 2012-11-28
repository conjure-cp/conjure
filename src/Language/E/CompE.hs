{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Language.E.CompE where

import Stuff.Funky.FunkySingle
import Stuff.Funky.FunkyMulti
import Stuff.NamedLog
import Stuff.MonadList

import Language.E.Imports
import Language.E.Definition
import Language.E.Pretty

import qualified Data.Set as S
import Data.IntMap ( IntMap )
import Data.IntSet ( IntSet )

import qualified GHC.Generics ( Generic )


class ( Functor m
      , Applicative m
      , Monad m
      , MonadState ConjureState m
      , MonadError ConjureError m
      ) => MonadConjure m where
    type ResultF m a
    runFunky :: m a -> ResultF m a


instance MonadConjure (FunkySingle ConjureState ConjureError Identity) where
    type ResultF      (FunkySingle ConjureState ConjureError Identity) a =
                      (Either ConjureError a, ConjureState)
    runFunky ma = runIdentity $ runFunkySingle def ma

instance MonadConjure (FunkyMulti  GlobalState ConjureState ConjureError Identity) where
    type ResultF      (FunkyMulti  GlobalState ConjureState ConjureError Identity) a =
                      [(Either ConjureError a, ConjureState)]
    runFunky ma = fst $ runIdentity $ runFunkyMulti def def ma


instance MonadConjure (FunkySingle ConjureState ConjureError IO) where
    type ResultF      (FunkySingle ConjureState ConjureError IO) a =
                      IO (Either ConjureError a, ConjureState)
    runFunky = runFunkySingle def

instance MonadConjure (FunkyMulti  GlobalState ConjureState ConjureError IO) where
    type ResultF      (FunkyMulti  GlobalState ConjureState ConjureError IO) a =
                      IO [(Either ConjureError a, ConjureState)]
    runFunky ma = liftM fst $ runFunkyMulti def def ma


class ( MonadConjure m
      , MonadList m
      ) => MonadConjureList m where
    getsGlobal :: (GlobalState -> a) -> m a
    modifyGlobal :: (GlobalState -> GlobalState) -> m ()


instance MonadConjureList (FunkyMulti GlobalState ConjureState ConjureError Identity) where
    getsGlobal = fmGetsGlobal
    modifyGlobal = fmModifyGlobal

instance MonadConjureList (FunkyMulti GlobalState ConjureState ConjureError IO) where
    getsGlobal = fmGetsGlobal
    modifyGlobal = fmModifyGlobal


runCompE
    :: String
    -> FunkyMulti GlobalState ConjureState ConjureError Identity a
    -> [(Either Doc a, LogTree)]
runCompE d ma = afterCompERun d $ runFunky ma


runCompEIO
    :: String
    -> FunkyMulti GlobalState ConjureState ConjureError IO a
    -> IO [(Either Doc a, LogTree)]
runCompEIO d ma = afterCompERun d <$> runFunky ma


runCompESingle
    :: String
    -> FunkySingle ConjureState ConjureError Identity a
    -> (Either Doc a, LogTree)
runCompESingle d ma = afterCompERunSingle d $ runFunky ma


runCompEIOSingle
    :: String
    -> FunkySingle ConjureState ConjureError IO a
    -> IO (Either Doc a, LogTree)
runCompEIOSingle d ma = afterCompERunSingle d <$> runFunky ma


afterCompERun
    :: String
    -> [(Either ConjureError a, ConjureState)]
    -> [(Either Doc          a, LogTree     )]
afterCompERun d = map (afterCompERunSingle d)

afterCompERunSingle
    :: String
    -> (Either ConjureError a, ConjureState)
    -> (Either Doc          a, LogTree     )
afterCompERunSingle d (Left  x, st) = (Left $ toError d x, localLogs st)
afterCompERunSingle _ (Right x, st) = (Right x           , localLogs st)

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

toError :: String -> ConjureError -> Doc
toError msg = prettyError (pretty $ "Error in phase: " ++ msg)



-- errors

type ConjureError = (ErrEnum, Doc, Maybe Spec)

data ErrEnum = ErrFatal        -- means execution cannot continue.
             | ErrGeneratesNone
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
        , representationLog :: [ ( Text     -- original name
                                 , Text     -- representation name
                                 , E        -- original full declaration
                                 , E        -- new domain
                                 ) ]
        , structuralConsLog :: [E]
        , lastSpec :: Maybe Spec  -- record the spec after changes, to report in case of an error.
        , localLogs :: LogTree
        , allNamesPreConjure :: S.Set Text  -- all identifiers used in the spec, pre conjure. to avoid name clashes.
        }

bindersDoc :: MonadConjure m => m Doc
bindersDoc = do
    bs <- gets binders
    return $
        "Current bindings: " <+>
        prettyList id "," (nub $ map binderName bs)


data Binder = Binder Text E
    deriving (Show, GHC.Generics.Generic)

binderName :: Binder -> Text
binderName (Binder nm _) = nm

instance Hashable Binder where
    hashWithSalt s x = gHashWithSalt s x
    {-# INLINEABLE hashWithSalt #-}

instance Default ConjureState where
    def = ConjureState def 1 def def def def def


mkLog :: MonadConjure m => String -> Doc -> m ()
mkLog nm doc = case buildLog nm doc of
    Nothing -> return ()
    Just l  -> modify $ \ st -> st {
        localLogs = LTMultiple (localLogs st) (LTSingle l)
        }

addReference :: MonadConjure m => Text -> E -> m ()
addReference nm val = modify $ \ st -> st { binders = Binder nm val : binders st }

addMetaVar :: MonadConjure m => Text -> E -> m ()
addMetaVar nm = addReference ("&" `mappend` nm)

lookupReference :: MonadConjure m => Text -> MaybeT m E
lookupReference nm = do
    let (base,_,_) = identifierSplit nm
    bs <- lift $ gets binders
    case listToMaybe [ x | Binder nm' x <- bs, base == nm' ] of
        Nothing -> mzero
        Just x  -> return x

lookupMetaVar :: MonadConjure m => Text -> MaybeT m E
lookupMetaVar nm = lookupReference ("&" `mappend` nm)

errUndefinedRef :: (MonadConjure m, Pretty p) => Doc -> p -> m a
errUndefinedRef place t = do
    bsText <- bindersDoc
    err ErrFatal $ vcat [ "(in" <+> place <> ")"
                        , "Undefined reference:" <+> pretty t
                        , nest 4 ("Current bindings:" <+> bsText)
                        ]

errMaybeT :: MonadConjure m => Doc -> (Text -> MaybeT m E) -> Text -> m E
errMaybeT place comp t = do
    ma <- runMaybeT (comp t)
    case ma of
        Just i  -> return i
        Nothing -> errUndefinedRef place t

nextUniqueName :: MonadConjure m => m Text
nextUniqueName = do
    i <- gets uniqueNameInt
    modify $ \ st -> st { uniqueNameInt = i + 1 }
    let nm = stringToText ("v__" ++ show i)
    nms <- gets allNamesPreConjure
    if nm `S.member` nms
        then nextUniqueName
        else return nm


data GlobalState = GlobalState
        { memoRefnChanged      :: IntMap E
        , memoRefnStaysTheSame :: IntSet
        }

instance Default GlobalState where
    def = GlobalState def def

makeIdempotent :: Monad m => (a -> m (a,Bool)) -> a -> m a
makeIdempotent f x = do
    (y,flag) <- f x
    if flag
        then makeIdempotent f y
        else return y



