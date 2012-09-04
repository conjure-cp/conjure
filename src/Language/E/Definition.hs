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
        }
    deriving ( Show )

data Binder = Binder String E
    deriving (Show)

instance Default LocalState where
    def = LocalState [] 1

data GlobalState = GlobalState
        { logs :: [NamedLog]
        }

instance Default GlobalState where
    def = GlobalState []

mkLog :: Monad m => String -> Doc -> CompE m ()
mkLog nm doc = case buildLog nm doc of
    Nothing -> return ()
    Just l  -> modifyGlobal $ \ st -> st { logs = logs st ++ [l] }

addBinder :: Monad m => String -> E -> CompE m ()
addBinder nm val = do
    case nm of
        '@':_ -> return ()
        _ -> mkLog "debug:addBinder" $ stringToDoc nm
    modifyLocal $ \ st -> st { binders = Binder nm val : binders st }

-- lookupBinder :: Monad m => String -> MaybeT (CompE m) E
lookupBinder nm = do
    bs <- lift $ getsLocal binders
    case listToMaybe [ x | Binder nm' x <- bs, nm == nm' ] of
        Nothing -> mzero
        Just x  -> return x

nextUniqueName :: Monad m => CompE m String
nextUniqueName = do
    !i <- getsLocal uniqueNameInt
    modifyLocal $ \ st -> st { uniqueNameInt = i + 1 }
    return $ "__" ++ show i
