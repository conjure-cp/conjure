{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.CompE where

import Stuff.FunkyT
import Stuff.NamedLog

import Language.E.Imports
import Language.E.Definition
import Language.E.Pretty

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.DList as DList
import System.IO.Unsafe ( unsafeInterleaveIO )


type CompEMOnly m = FunkyT LocalState GlobalState (CompError, Maybe Spec) m
type CompE m a = CompEMOnly m a

runCompE
    :: Monad m
    => StdGen
    -> CompE m a
    -> m ([(Either (CompError, Maybe Spec) a, LocalState)], (GlobalState, StdGen))
runCompE gen = runFunkyT def (def, gen)

runCompEIO :: CompE Identity a -> IO [a]
runCompEIO comp = do
    gen <- getStdGen
    let (mgenerateds, _) = runIdentity $ runCompE gen comp
    forM mgenerateds $ \ mgenerated -> case mgenerated of
        (Left  x, locSt) -> do
            printLogs (localLogs locSt)
            error $ renderPretty $ prettyErrors "There were errors." [x]
        (Right x, locSt) -> do
            printLogs (localLogs locSt)
            unsafeInterleaveIO $ return x



-- errors

type CompError = (ErrEnum, Doc)

data ErrEnum = ErrFatal        -- means execution cannot continue.
    deriving (Eq, Show)

err :: Monad m => ErrEnum -> Doc -> CompE m a
err e d = do
    sp <- getsLocal lastSpec
    throwError ((e,d), sp)

prettyErrors :: Doc -> [(CompError, Maybe Spec)] -> Doc
prettyErrors msg es = vcat $ msg : map (nest 4 . one) es
    where
        one ((e,d), Nothing) = stringToDoc (show e) <+> d
        one ((e,d), Just sp) = vcat [ stringToDoc (show e) <+> d
                                    , pretty sp
                                    -- , prettySpecDebug sp
                                    ]

recordSpec :: Monad m => Spec -> CompE m Spec
recordSpec sp = do
    modifyLocal $ \ st -> st { lastSpec = Just sp }
    return sp


-- state

data LocalState = LocalState
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
        , localLogs :: DList.DList NamedLog
        , allNamesPreConjure :: S.Set String  -- all identifiers used in the spec, pre conjure. to avoid name clashes.
        }

data Binder = Binder String E
    deriving (Show)

instance Default LocalState where
    def = LocalState def 1 def def def def DList.empty def

data GlobalState = GlobalState

instance Default GlobalState where
    def = GlobalState

mkLog :: Monad m => String -> Doc -> CompE m ()
mkLog nm doc = case buildLog nm doc of
    Nothing -> return ()
    Just l  -> modifyLocal $ \ st -> st { localLogs = localLogs st `DList.snoc` l }

addBinder :: Monad m => String -> E -> CompE m ()
addBinder nm val = modifyLocal $ \ st -> st { binders = Binder nm val : binders st }

lookupBinder :: Monad m => String -> MaybeT (FunkyT LocalState GlobalState (CompError, Maybe Spec) m) E
lookupBinder nm = do
    bs <- lift $ getsLocal binders
    case listToMaybe [ x | Binder nm' x <- bs, nm == nm' ] of
        Nothing -> mzero
        Just x  -> return x

nextUniqueName :: Monad m => CompE m String
nextUniqueName = do
    i <- getsLocal uniqueNameInt
    modifyLocal $ \ st -> st { uniqueNameInt = i + 1 }
    let nm = "v__" ++ show i
    nms <- getsLocal allNamesPreConjure
    if nm `S.member` nms
        then nextUniqueName
        else return nm


makeIdempotent :: Monad m => (a -> CompE m (a,Bool)) -> a -> CompE m a
makeIdempotent f x = do
    (y,flag) <- f x
    if flag
        then makeIdempotent f y
        else return y



