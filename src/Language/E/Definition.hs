{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.E.Definition
    ( module Stuff.Generic

    , Spec(..), Version, E, BuiltIn(..)
    , RuleRefn, RuleRepr, RuleReprCase, RuleReprResult

    , listAsStatement, statementAsList

    ) where

import Stuff.Generic
import Stuff.Pretty
import Stuff.MetaVariable

import Language.E.Imports



data Spec = Spec Version E
    deriving (Eq, Show)

instance Default Spec where
    def = Spec ("Essence", [1,3]) (Tagged TstatementEOF [])

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

type RuleReprResult = ( E            -- original declaration
                      , String       -- rule name
                      , String       -- name of the representation
                      , E            -- replacement domain
                      , [E]          -- structural constraints
                      )

type E = Generic BuiltIn

data BuiltIn = B !Bool | I !Integer | S String
    deriving (Eq, Ord, Show)

instance Pretty BuiltIn where
    pretty (B x) = pretty x
    pretty (I x) = pretty x
    pretty (S x) = pretty x

instance MetaVariable E where
    unnamedMV [xMatch| [Prim (S "_")] := reference |] = True
    unnamedMV _ = False
    namedMV   [xMatch| [Prim (S  s )] := metavar   |] = Just s
    namedMV   _ = Nothing



listAsStatement :: [E] -> E
listAsStatement []     = [xMake| statementEOF   := [] |]
listAsStatement (x:xs) = [xMake| statement.this := [x]
                               | statement.next := [listAsStatement xs]
                               |]

statementAsList :: E -> [E]
statementAsList [xMatch| _ := statementEOF |] = []
statementAsList [xMatch| [this] := statement.this
                       | [next] := statement.next
                       |] = this : statementAsList next
statementAsList x = [x]

