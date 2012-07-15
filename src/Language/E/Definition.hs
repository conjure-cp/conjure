{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.E.Definition
    ( module Stuff.Generic

    , Spec(..), Version, RuleRefn, E, BuiltIn(..)

    , CompE, runCompE

    , CompState(..), Binder(..)
    , addBinder, lookupBinder, nextUniqueName

    , CompError, ErrEnum(..)
    , err, prettyErrors

    ) where

import Stuff.Generic
import Stuff.Pretty
import Stuff.MetaVariable
import Stuff.CompT
import Stuff.NamedLog
import Stuff.MonadList

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



newtype CompE m a = CompE (CompT () CompState [NamedLog] CompError m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState CompState
             , MonadWriter [NamedLog]
             , MonadError CompError
             , MonadList
             , MonadIO
             )

runCompE :: Monad m => CompE m a -> m [(Either CompError a, CompState, [NamedLog])]
runCompE (CompE compt) = runCompT def def compt


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

data CompState = CompState { binders :: [Binder]
                           , uniqueNameInt :: Integer }
    deriving ( Show )

data Binder = Binder String E
    deriving (Show)

instance Default CompState where
    def = CompState [] 1

addBinder :: Monad m => String -> E -> CompE m ()
addBinder nm val = modify $ \ st -> st { binders = Binder nm val : binders st }

lookupBinder :: Monad m => String -> MaybeT (CompE m) E
lookupBinder nm = do
    bs <- lift $ gets binders
    case listToMaybe [ x | Binder nm' x <- bs, nm == nm' ] of
        Nothing -> mzero
        Just x  -> return x

nextUniqueName :: Monad m => CompE m String
nextUniqueName = do
    !i <- gets uniqueNameInt
    modify $ \ st -> st { uniqueNameInt = i + 1 }
    return $ "__" ++ show i
