{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.E.Definition
    ( module Stuff.Generic

    , Spec(..), Version, E, BuiltIn(..)
    , RulesDB, RuleRefn, RuleRepr, RuleReprCase, RuleReprResult

    , listAsStatement, statementAsList

    , identifierSplit, identifierConstruct
    , identifierStripRegion

    ) where

import Stuff.Generic
import Stuff.Pretty
import Stuff.MetaVariable

import Language.E.Imports

import qualified Data.Text as T
import qualified GHC.Generics ( Generic )



data Spec = Spec Version E
    deriving (Eq, Show, GHC.Generics.Generic)

instance Serialize Spec

instance Hashable Spec where

instance NFData Spec where
    rnf x = genericRnf x
    {-# INLINEABLE rnf #-}

instance Default Spec where
    def = Spec ("Essence", [1,3]) (Tagged TstatementEOF [])

type Version = (Text,[Int])

type RulesDB = ([RuleRepr], [RuleRefn])

type RuleRefn = (Text, Maybe Int, E)
type RuleRepr = ( Text          -- name of the rule
                , Text          -- name of the representation
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
                      , Text         -- rule name
                      , Text         -- name of the representation
                      , E            -- replacement domain
                      , [E]          -- structural constraints
                      )

type E = Generic BuiltIn

data BuiltIn = B !Bool | I !Integer | S !Text
    deriving (Eq, Ord, Show, GHC.Generics.Generic)

instance Serialize BuiltIn

instance Hashable BuiltIn where

instance NFData BuiltIn where
    rnf x = genericRnf x
    {-# INLINEABLE rnf #-}

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
statementAsList [xMatch| []     := statement.this.statementEOF
                       | [next] := statement.next
                       |] = statementAsList next
statementAsList [xMatch| [this] := statement.this
                       | [next] := statement.next
                       |] = this : statementAsList next
statementAsList x = [x]

identifierSplit :: Text -> (Text, Maybe Text, Maybe Text)
identifierSplit t =
    case T.splitOn "§" t of
        [base, rest] -> case T.splitOn "#" rest of
            [region, repr] -> (base, Just region, Just repr)
            _              -> (base, Just rest  , Nothing  )
        _            -> case T.splitOn "#" t of
            [base  , repr] -> (base, Nothing    , Just repr)
            _              -> (t   , Nothing    , Nothing  )

identifierConstruct :: Text -> Maybe Text -> Maybe Text -> Text
identifierConstruct base mregion mrepr =
    mconcat [ base
            , maybe mempty ("§" `mappend`) mregion
            , maybe mempty ("#" `mappend`) mrepr
            ]

identifierStripRegion :: Text -> Text
identifierStripRegion t =
    let (base, _, refn) = identifierSplit t
    in  identifierConstruct base Nothing refn


