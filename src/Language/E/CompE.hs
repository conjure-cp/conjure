{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.CompE where

import Stuff.Generic
import Stuff.FunkyT
import Stuff.NamedLog

import Language.E.Imports
import Language.E.Definition
import Language.E.Pretty

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.DList as DList
import System.IO.Unsafe ( unsafeInterleaveIO )



type CompE m a = FunkyT LocalState GlobalState (CompError, Maybe Spec) m a

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
        }

data Binder = Binder String E
    deriving (Show)

instance Default LocalState where
    def = LocalState def 1 def def def def DList.empty

data GlobalState = GlobalState
        { allNamesPreConjure :: S.Set String  -- all identifiers used in the spec, pre conjure. to avoid name clashes.
        }

instance Default GlobalState where
    def = GlobalState def

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
    nms <- getsGlobal allNamesPreConjure
    if nm `S.member` nms
        then nextUniqueName
        else return nm


makeIdempotent :: Monad m => (a -> CompE m (a,Bool)) -> a -> CompE m a
makeIdempotent f x = do
    (y,flag) <- f x
    if flag
        then makeIdempotent f y
        else return y



-- much needed

processStatement :: Monad m => E -> CompE m ()

processStatement s@[xMatch| [Prim (S name)] := topLevel.declaration.find.name.reference
                          | [      _      ] := topLevel.declaration.find.domain
                          |] = addBinder name s

processStatement s@[xMatch| [Prim (S name)] := topLevel.declaration.given.name.reference
                          | [      _      ] := topLevel.declaration.given.domain
                          |] = addBinder name s

processStatement   [xMatch| [Prim (S name)] := topLevel.letting.name.reference
                          | [ val ]         := topLevel.letting.expr
                          |] = addBinder name val
processStatement   [xMatch| [Prim (S name)] := topLevel.letting.name.metavar
                          | [ val ]         := topLevel.letting.expr
                          |] = addBinder ('&':name) val

processStatement   [xMatch| [Prim (S name)] := topLevel.letting.name.reference
                          | [ val ]         := topLevel.letting.domain
                          |] = addBinder name val
processStatement   [xMatch| [Prim (S name)] := topLevel.letting.name.metavar
                          | [ val ]         := topLevel.letting.domain
                          |] = addBinder ('&':name) val

processStatement   [xMatch| _ := topLevel.suchThat  |] = return ()
processStatement   [xMatch| _ := topLevel.objective |] = return ()
processStatement   [xMatch| _ := topLevel.where     |] = return ()

processStatement s@[xMatch| _ := topLevel           |] = mkLog "missing:processStatement" $ "not handled in processStatement" <+> prettyAsPaths s
processStatement s = mkLog "missing:processStatement" $ "not handled in processStatement" <+> prettyAsPaths s
