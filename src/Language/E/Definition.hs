{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.E.Definition
    ( module Stuff.Generic

    , Spec(..), Version, E, BuiltIn(..)
    , RuleRefn, RuleRepr, RuleReprCase

    , CompE, runCompE

    , LocalState(..), GlobalState(..), Binder(..)
    , addBinder, lookupBinder, nextUniqueName, mkLog
    , processStatement

    , CompError, ErrEnum(..)
    , err, prettyErrors

    ) where

import Stuff.Generic
import Stuff.Pretty
import Stuff.MetaVariable
import Stuff.FunkyT
import Stuff.NamedLog

import Language.E.Imports

import Data.Generics ( Data, Typeable )
import qualified Data.Set as S

-- import Control.Monad ( mzero )
-- import Control.Monad.Trans ( lift )
-- import Control.Monad.Trans.Maybe ( MaybeT )
-- import Control.Monad.State ( gets, modify )
-- import Data.Default ( Default(..) )
-- import Data.Maybe ( listToMaybe )

-- import Text.PrettyPrint ( Doc )



data Spec = Spec Version [E]
    deriving (Eq, Show)

type Version = (String,[Int])

type RuleRefn = (String, Maybe Int, E)
type RuleRepr = ( String        -- name of the rule
                , String        -- name of the representation
                , E             -- domain out.
                , Maybe E       -- structural constraints
                , [E]           -- locals
                , [RuleReprCase]
                )
type RuleReprCase = ( E         -- domain in.
                    , Maybe E   -- structural constraints
                    , [E]       -- locals
                    )

type E = Generic BuiltIn

data BuiltIn = B Bool | I Integer | S String
    deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty BuiltIn where
    pretty (B x) = pretty x
    pretty (I x) = pretty x
    pretty (S x) = pretty x

instance MetaVariable E where
    unnamedMV [xMatch| [Prim (S "_")] := reference |] = True
    unnamedMV _ = False
    namedMV   [xMatch| [Prim (S  s )] := metavar   |] = Just s
    namedMV   _ = Nothing



type CompE m a = FunkyT LocalState GlobalState CompError m a

runCompE :: Monad m => CompE m a -> m ([(Either CompError a, LocalState)], GlobalState)
runCompE comp = runFunkyT def def comp


-- errors

type CompError = (ErrEnum, Doc)

data ErrEnum = ErrFatal        -- means execution cannot continue.
    deriving (Eq, Show)

err :: Monad m => ErrEnum -> Doc -> CompE m a
err e d = throwError (e,d)

prettyErrors :: Doc -> [CompError] -> Doc
prettyErrors msg es = vcat $ msg : map (nest 4 . one) es
    where one (e,d) = stringToDoc (show e) <+> d


-- state

data LocalState = LocalState
        { binders       :: [Binder]
        , uniqueNameInt :: Integer
        , representationLog :: [ ( String   -- original name
                                 , String   -- representation name
                                 , E        -- original full declaration
                                 , E        -- new domain
                                 ) ]
        , structuralConsLog :: [E]
        }
    deriving ( Show )

data Binder = Binder String E
    deriving (Show)

instance Default LocalState where
    def = LocalState [] 1 [] []

data GlobalState = GlobalState
        { logs               :: [NamedLog]                  -- logs about execution
        , allNamesPreConjure :: S.Set String                -- all identifiers used in the spec, pre conjure. to avoid name clashes.
        }

instance Default GlobalState where
    def = GlobalState [] S.empty

mkLog :: Monad m => String -> Doc -> CompE m ()
mkLog nm doc = case buildLog nm doc of
    Nothing -> return ()
    Just l  -> modifyGlobal $ \ st -> st { logs = logs st ++ [l] }

addBinder :: Monad m => String -> E -> CompE m ()
addBinder nm val = do
    -- case nm of
    --     '&':_ -> return ()
    --     _ -> mkLog "addBinder" $ stringToDoc nm
    modifyLocal $ \ st -> st { binders = Binder nm val : binders st }

lookupBinder :: Monad m => String -> MaybeT (FunkyT LocalState GlobalState CompError m) E
lookupBinder nm = do
    bs <- lift $ getsLocal binders
    case listToMaybe [ x | Binder nm' x <- bs, nm == nm' ] of
        Nothing -> mzero
        Just x  -> return x

nextUniqueName :: Monad m => CompE m String
nextUniqueName = do
    !i <- getsLocal uniqueNameInt
    modifyLocal $ \ st -> st { uniqueNameInt = i + 1 }
    let !nm = "__" ++ show i
    nms <- getsGlobal allNamesPreConjure
    if nm `S.member` nms
        then nextUniqueName
        else return $! nm


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

processStatement s@[xMatch| _ := topLevel           |] = mkLog "processStatement" $ "not handled in processStatement" <+> prettyAsPaths s
processStatement s = mkLog "processStatement" $ "not handled in processStatement" <+> prettyAsPaths s

-- processStatement s@[xMatch| _ := topLevel           |] = err ErrFatal $ "not handled in processStatement" <+> prettyAsPaths s
-- processStatement s = err ErrFatal $ "not handled in processStatement" <+> prettyAsPaths s

-- processStatement _ = return ()

